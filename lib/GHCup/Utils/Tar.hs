{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : GHCup.Utils.Tar
Description : GHCup tar abstractions
Copyright   : (c) Julian Ospald, 2024
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Utils.Tar where

import           GHCup.Types
import           GHCup.Errors
import           GHCup.Prelude
import           GHCup.Prelude.Logger.Internal
import           GHCup.Types.Optics

import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Reader
import           Data.List
import           Haskus.Utils.Variant.Excepts
import           System.FilePath

#if defined(TAR)
import           Codec.Archive.Zip
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar
#else
import           Codec.Archive           hiding ( Directory )
#endif

import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T


-- | Unpack an archive to a given directory.
unpackToDir :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m)
            => FilePath       -- ^ destination dir
            -> FilePath       -- ^ archive path
            -> Excepts '[UnknownArchive
                        , ArchiveResult
                        ] m ()
unpackToDir dfp av = do
  let fn = takeFileName av
  lift $ logInfo $ "Unpacking: " <> T.pack fn <> " to " <> T.pack dfp

#if defined(TAR)
  let untar :: MonadIO m => BL.ByteString -> Excepts '[ArchiveResult] m ()
      untar = liftIO . Tar.unpack dfp . Tar.read

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#else
  let untar :: MonadIO m => BL.ByteString -> Excepts '[ArchiveResult] m ()
      untar = lEM . liftIO . runArchiveM . unpackToDirLazy dfp

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#endif

  -- extract, depending on file extension
  if
    | ".tar.gz" `isSuffixOf` fn -> liftE
      (untar . GZip.decompress =<< rf av)
    | ".tar.xz" `isSuffixOf` fn -> do
      filecontents <- liftE $ rf av
      let decompressed = Lzma.decompressWith (Lzma.defaultDecompressParams { Lzma.decompressAutoDecoder= True }) filecontents
      liftE $ untar decompressed
    | ".tar.bz2" `isSuffixOf` fn ->
      liftE (untar . BZip.decompress =<< rf av)
    | ".tar" `isSuffixOf` fn -> liftE (untar =<< rf av)
#if defined(TAR)
    | ".zip" `isSuffixOf` fn -> withArchive av (unpackInto dfp)
#else
    -- libarchive supports zip
    | ".zip" `isSuffixOf` fn -> liftE (untar =<< rf av)
#endif
    | otherwise -> throwE $ UnknownArchive fn


-- | Get all files from an archive.
getArchiveFiles :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m)
                => FilePath       -- ^ archive path
                -> Excepts '[ UnknownArchive
                            , ArchiveResult
                            ] m [FilePath]
getArchiveFiles av = do
  let fn = takeFileName av
#if defined(TAR)
  let entries :: Monad m => BL.ByteString -> Excepts '[ArchiveResult] m [FilePath]
      entries =
          lE @ArchiveResult
          . Tar.foldEntries
            (\e x -> fmap (Tar.entryTarPath e :) x)
            (Right [])
            (\_ -> Left ArchiveFailed)
          . Tar.decodeLongNames
          . Tar.read

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#else
  let entries :: Monad m => BL.ByteString -> Excepts '[ArchiveResult] m [FilePath]
      entries = (fmap . fmap) filepath . lE . readArchiveBSL

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#endif

  -- extract, depending on file extension
  if
    | ".tar.gz" `isSuffixOf` fn -> liftE
      (entries . GZip.decompress =<< rf av)
    | ".tar.xz" `isSuffixOf` fn -> do
      filecontents <- liftE $ rf av
      let decompressed = Lzma.decompressWith (Lzma.defaultDecompressParams { Lzma.decompressAutoDecoder= True }) filecontents
      liftE $ entries decompressed
    | ".tar.bz2" `isSuffixOf` fn ->
      liftE (entries . BZip.decompress =<< rf av)
    | ".tar" `isSuffixOf` fn -> liftE (entries =<< rf av)
    | ".zip" `isSuffixOf` fn ->
#if defined(TAR)
        withArchive av $ do
          entries' <- getEntries
          pure $ fmap unEntrySelector $ Map.keys entries'
#else
        liftE (entries =<< rf av)
#endif
    | otherwise -> throwE $ UnknownArchive fn

