module Codec.Archive.Pack ( entriesToFile
                          , entriesToFileZip
                          , entriesToFile7Zip
                          , entriesToFileCpio
                          , entriesToFileXar
                          , entriesToBS
                          , entriesToBSzip
                          , entriesToBS7zip
                          , packEntries
                          , noFail
                          , packToFile
                          , packToFileZip
                          , packToFile7Zip
                          , packToFileCpio
                          , packToFileXar
                          ) where

import           Codec.Archive.Foreign
import           Codec.Archive.Monad
import           Codec.Archive.Pack.Common
import           Codec.Archive.Types
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.ByteString           (packCStringLen)
import qualified Data.ByteString           as BS
import           Data.Coerce               (coerce)
import           Data.Foldable             (sequenceA_, traverse_)
import           Data.Semigroup            (Sum (..))
import           Foreign.C.String
import           Foreign.C.Types           (CLLong (..), CLong (..))
import           Foreign.Ptr               (Ptr)
import           System.IO.Unsafe          (unsafeDupablePerformIO)

maybeDo :: Applicative f => Maybe (f ()) -> f ()
maybeDo = sequenceA_

contentAdd :: EntryContent -> Ptr Archive -> Ptr ArchiveEntry -> ArchiveM ()
contentAdd (NormalFile contents) a entry = do
    liftIO $ archiveEntrySetFiletype entry (Just FtRegular)
    liftIO $ archiveEntrySetSize entry (fromIntegral (BS.length contents))
    handle $ archiveWriteHeader a entry
    useAsCStringLenArchiveM contents $ \(buff, sz) ->
        liftIO $ void $ archiveWriteData a buff (fromIntegral sz)
contentAdd Directory a entry = do
    liftIO $ archiveEntrySetFiletype entry (Just FtDirectory)
    handle $ archiveWriteHeader a entry
contentAdd (Symlink fp st) a entry = do
    liftIO $ archiveEntrySetFiletype entry (Just FtLink)
    liftIO $ archiveEntrySetSymlinkType entry st
    liftIO $ withCString fp $ \fpc ->
        archiveEntrySetSymlink entry fpc
    handle $ archiveWriteHeader a entry
contentAdd (Hardlink fp) a entry = do
    liftIO $ archiveEntrySetFiletype entry Nothing
    liftIO $ withCString fp $ \fpc ->
        archiveEntrySetHardlink entry fpc
    handle $ archiveWriteHeader a entry

withMaybeCString :: Maybe String -> (Maybe CString -> IO a) -> IO a
withMaybeCString (Just x) f = withCString x (f . Just)
withMaybeCString Nothing f  = f Nothing

setOwnership :: Ownership -> Ptr ArchiveEntry -> IO ()
setOwnership (Ownership uname gname uid gid) entry =
    withMaybeCString uname $ \unameC ->
    withMaybeCString gname $ \gnameC ->
    traverse_ maybeDo
        [ archiveEntrySetUname entry <$> unameC
        , archiveEntrySetGname entry <$> gnameC
        , Just (archiveEntrySetUid entry (coerce uid))
        , Just (archiveEntrySetGid entry (coerce gid))
        ]

setTime :: ModTime -> Ptr ArchiveEntry -> IO ()
setTime (time', nsec) entry = archiveEntrySetMtime entry time' nsec

packEntries :: (Foldable t) => Ptr Archive -> t Entry -> ArchiveM ()
packEntries a = traverse_ (archiveEntryAdd a)

-- Get a number of bytes appropriate for creating the archive.
entriesSz :: (Foldable t, Integral a) => t Entry -> a
entriesSz = getSum . foldMap (Sum . entrySz)
    where entrySz e = 512 + 512 * (contentSz (content e) `div` 512 + 1)
          contentSz (NormalFile str) = fromIntegral $ BS.length str
          contentSz Directory        = 0
          contentSz (Symlink fp _)   = 1 + fromIntegral (length fp)
          contentSz (Hardlink fp)    = fromIntegral $ length fp --idk if this is right

-- | Returns a 'BS.ByteString' containing a tar archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBS :: Foldable t => t Entry -> BS.ByteString
entriesToBS = unsafeDupablePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormatPaxRestricted
{-# NOINLINE entriesToBS #-}

-- | Returns a 'BS.ByteString' containing a @.7z@ archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBS7zip :: Foldable t => t Entry -> BS.ByteString
entriesToBS7zip = unsafeDupablePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormat7zip
{-# NOINLINE entriesToBS7zip #-}

-- | Returns a 'BS.ByteString' containing a zip archive with the 'Entry's
--
-- @since 1.0.0.0
entriesToBSzip :: Foldable t => t Entry -> BS.ByteString
entriesToBSzip = unsafeDupablePerformIO . noFail . entriesToBSGeneral archiveWriteSetFormatZip
{-# NOINLINE entriesToBSzip #-}

-- This is for things we don't think will fail. When making a 'BS.ByteString'
-- from a bunch of 'Entry's, for instance, we don't anticipate any errors
noFail :: ArchiveM a -> IO a
noFail act = do
    res <- runArchiveM act
    case res of
        Right x -> pure x
        Left _  -> error "Should not fail."

-- | Internal function to be used with 'archive_write_set_format_pax' etc.
entriesToBSGeneral :: (Foldable t) => (Ptr Archive -> IO ArchiveResult) -> t Entry -> ArchiveM BS.ByteString
entriesToBSGeneral modifier hsEntries' = do
    a <- liftIO archiveWriteNew
    ignore $ modifier a
    allocaBytesArchiveM bufSize $ \buffer -> do
        (err, usedSz) <- liftIO $ archiveWriteOpenMemory a buffer bufSize
        handle (pure err)
        packEntries a hsEntries'
        handle $ archiveWriteClose a
        res <- liftIO $ curry packCStringLen buffer (fromIntegral usedSz)
        ignore $ archiveFree a
        pure res

    where bufSize :: Integral a => a
          bufSize = entriesSz hsEntries'

filePacker :: (Traversable t) => (FilePath -> t Entry -> ArchiveM ()) -> FilePath -> t FilePath -> ArchiveM ()
filePacker f tar fps = f tar =<< liftIO (traverse mkEntry fps)

-- | @since 2.0.0.0
packToFile :: Traversable t
           => FilePath -- ^ @.tar@ archive to be created
           -> t FilePath -- ^ Files to include
           -> ArchiveM ()
packToFile = filePacker entriesToFile

-- | @since 2.0.0.0
packToFileZip :: Traversable t
              => FilePath
              -> t FilePath
              -> ArchiveM ()
packToFileZip = filePacker entriesToFileZip

-- | @since 2.0.0.0
packToFile7Zip :: Traversable t
               => FilePath
               -> t FilePath
               -> ArchiveM ()
packToFile7Zip = filePacker entriesToFile7Zip

-- | @since 2.2.3.0
packToFileCpio :: Traversable t
               => FilePath
               -> t FilePath
               -> ArchiveM ()
packToFileCpio = filePacker entriesToFileCpio

-- | @since 2.2.4.0
packToFileXar :: Traversable t
              => FilePath
              -> t FilePath
              -> ArchiveM ()
packToFileXar = filePacker entriesToFileXar

-- | Write some entries to a file, creating a tar archive. This is more
-- efficient than
--
-- @
-- BS.writeFile "file.tar" (entriesToBS entries)
-- @
--
-- @since 1.0.0.0
entriesToFile :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFile = entriesToFileGeneral archiveWriteSetFormatPaxRestricted
-- this is the recommended format; it is a tar archive

-- | Write some entries to a file, creating a zip archive.
--
-- @since 1.0.0.0
entriesToFileZip :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFileZip = entriesToFileGeneral archiveWriteSetFormatZip

-- | Write some entries to a file, creating a @.7z@ archive.
--
-- @since 1.0.0.0
entriesToFile7Zip :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFile7Zip = entriesToFileGeneral archiveWriteSetFormat7zip

-- | Write some entries to a file, creating a @.cpio@ archive.
--
-- @since 2.2.3.0
entriesToFileCpio :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFileCpio = entriesToFileGeneral archiveWriteSetFormatCpio

-- | Write some entries to a file, creating a @.xar@ archive.
--
-- @since 2.2.4.0
entriesToFileXar :: Foldable t => FilePath -> t Entry -> ArchiveM ()
entriesToFileXar = entriesToFileGeneral archiveWriteSetFormatXar

entriesToFileGeneral :: Foldable t => (Ptr Archive -> IO ArchiveResult) -> FilePath -> t Entry -> ArchiveM ()
entriesToFileGeneral modifier fp hsEntries' =
    bracketM
        archiveWriteNew
        archiveFree
        (\a -> do
            ignore $ modifier a
            withCStringArchiveM fp $ \fpc ->
                handle $ archiveWriteOpenFilename a fpc
            packEntries a hsEntries')

withArchiveEntry :: (Ptr ArchiveEntry -> ArchiveM a) -> ArchiveM a
withArchiveEntry =
    bracketM
        archiveEntryNew
        archiveEntryFree

archiveEntryAdd :: Ptr Archive -> Entry -> ArchiveM ()
archiveEntryAdd a (Entry fp contents perms owner mtime) =
    withArchiveEntry $ \entry -> do
        liftIO $ withCString fp $ \fpc ->
            archiveEntrySetPathname entry fpc
        liftIO $ archiveEntrySetPerm entry perms
        liftIO $ setOwnership owner entry
        liftIO $ maybeDo (setTime <$> mtime <*> pure entry)
        contentAdd contents a entry
