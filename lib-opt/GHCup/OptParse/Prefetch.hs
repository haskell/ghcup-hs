{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Prefetch where


import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils.Parsers (fromVersion)
import           GHCup.Types.Optics
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.OptParse.Common

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import GHCup.Download (getDownloadsF)




    ----------------
    --[ Commands ]--
    ----------------


data PrefetchCommand = PrefetchGHC PrefetchGHCOptions (Maybe ToolVersion)
                     | PrefetchCabal PrefetchOptions (Maybe ToolVersion)
                     | PrefetchHLS PrefetchOptions (Maybe ToolVersion)
                     | PrefetchStack PrefetchOptions (Maybe ToolVersion)
                     | PrefetchMetadata





    ---------------
    --[ Options ]--
    ---------------


data PrefetchOptions = PrefetchOptions {
  pfCacheDir :: Maybe FilePath
}

data PrefetchGHCOptions = PrefetchGHCOptions {
    pfGHCSrc :: Bool
  , pfGHCCacheDir :: Maybe FilePath
}



    ---------------
    --[ Parsers ]--
    ---------------


prefetchP :: Parser PrefetchCommand
prefetchP = subparser
  (  command
      "ghc"
      (info
        (PrefetchGHC
          <$> (PrefetchGHCOptions
                <$> ( switch (short 's' <> long "source" <> help "Download source tarball instead of bindist") <**> helper )
                <*> optional (option str (short 'd' <> long "directory" <> help "directory to download into (default: ~/.ghcup/cache/)" <> completer (bashCompleter "directory"))))
          <*>  optional (toolVersionTagArgument [] (Just GHC)) )
        ( progDesc "Download GHC assets for installation")
      )
      <>
     command
      "cabal"
      (info
        (PrefetchCabal
          <$> fmap PrefetchOptions (optional (option str (short 'd' <> long "directory" <> help "directory to download into (default: ~/.ghcup/cache/)" <> completer (bashCompleter "directory"))))
          <*> ( optional (toolVersionTagArgument [] (Just Cabal)) <**> helper ))
        ( progDesc "Download cabal assets for installation")
      )
      <>
     command
      "hls"
      (info
        (PrefetchHLS
          <$> fmap PrefetchOptions (optional (option str (short 'd' <> long "directory" <> help "directory to download into (default: ~/.ghcup/cache/)" <> completer (bashCompleter "directory"))))
          <*> ( optional (toolVersionTagArgument [] (Just HLS)) <**> helper ))
        ( progDesc "Download HLS assets for installation")
      )
      <>
     command
      "stack"
      (info
        (PrefetchStack
          <$> fmap PrefetchOptions (optional (option str (short 'd' <> long "directory" <> help "directory to download into (default: ~/.ghcup/cache/)" <> completer (bashCompleter "directory"))))
          <*> ( optional (toolVersionTagArgument [] (Just Stack)) <**> helper ))
        ( progDesc "Download stack assets for installation")
      )
      <>
     command
      "metadata"
      (PrefetchMetadata <$ info
        helper
        ( progDesc "Download ghcup's metadata, needed for various operations")
      )
  )



    --------------
    --[ Footer ]--
    --------------


prefetchFooter :: String
prefetchFooter = [s|Discussion:
  Prefetches tools or assets into "~/.ghcup/cache" directory. This can
  be then combined later with '--offline' flag, ensuring all assets that
  are required for offline use have been prefetched.

Examples:
  ghcup prefetch metadata
  ghcup prefetch ghc 8.10.5
  ghcup --offline install ghc 8.10.5|]



    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type PrefetchEffects = '[ TagNotFound
                        , DayNotFound
                        , NextVerNotFound
                        , NoToolVersionSet
                        , NoDownload
                        , DigestError
                        , ContentLengthError
                        , GPGError
                        , DownloadFailed
                        , JSONError
                        , FileDoesNotExistError
                        , StackPlatformDetectError
                        ]


runPrefetch :: MonadUnliftIO m
            => (ReaderT AppState m (VEither PrefetchEffects a) -> m (VEither PrefetchEffects a))
            -> Excepts PrefetchEffects (ResourceT (ReaderT AppState m)) a
            -> m (VEither PrefetchEffects a)
runPrefetch runAppState =
  runAppState
  . runResourceT
  . runE
    @PrefetchEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



prefetch :: ( Monad m
            , MonadMask m
            , MonadUnliftIO m
            , MonadFail m
            )
         => PrefetchCommand
         -> (forall a. ReaderT AppState m (VEither PrefetchEffects a) -> m (VEither PrefetchEffects a))
         -> (ReaderT LeanAppState m () -> m ())
         -> m ExitCode
prefetch prefetchCommand runAppState runLogger =
  runPrefetch runAppState (do
    case prefetchCommand of
      PrefetchGHC
        (PrefetchGHCOptions pfGHCSrc pfCacheDir) mt -> do
          forM_ pfCacheDir (liftIO . createDirRecursive')
          (v, _) <- liftE $ fromVersion mt GHC
          if pfGHCSrc
          then liftE $ fetchGHCSrc v pfCacheDir
          else liftE $ fetchToolBindist v GHC pfCacheDir
      PrefetchCabal PrefetchOptions {pfCacheDir} mt   -> do
        forM_ pfCacheDir (liftIO . createDirRecursive')
        (v, _) <- liftE $ fromVersion mt Cabal
        liftE $ fetchToolBindist v Cabal pfCacheDir
      PrefetchHLS PrefetchOptions {pfCacheDir} mt   -> do
        forM_ pfCacheDir (liftIO . createDirRecursive')
        (v, _) <- liftE $ fromVersion mt HLS
        liftE $ fetchToolBindist v HLS pfCacheDir
      PrefetchStack PrefetchOptions {pfCacheDir} mt   -> do
        forM_ pfCacheDir (liftIO . createDirRecursive')
        (v, _) <- liftE $ fromVersion mt Stack
        liftE $ fetchToolBindist v Stack pfCacheDir
      PrefetchMetadata -> do
        pfreq <- lift getPlatformReq
        _ <- liftE $ getDownloadsF pfreq
        pure ""
       ) >>= \case
                VRight _ -> do
                      pure ExitSuccess
                VLeft e -> do
                  runLogger $ logError $ T.pack $ prettyHFError e
                  pure $ ExitFailure 15
