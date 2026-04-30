{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.OptParse.Upgrade where




import GHCup.Command.Upgrade
import GHCup.Errors
import GHCup.Prelude.File
import GHCup.Prelude.Logger
import GHCup.Query.Metadata
import GHCup.Types
import GHCup.Types.Optics

import Control.Concurrent ( threadDelay )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Exception.Safe       ( MonadMask )
import Control.Monad                ( forM_ )
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions
import Options.Applicative          hiding ( style )
import Prelude                      hiding ( appendFile )
import System.Environment
import System.Exit
import System.FilePath

import qualified Data.Text as T






    ---------------
    --[ Options ]--
    ---------------


data UpgradeOpts
  = UpgradeInplace
  | UpgradeAt FilePath
  | UpgradeGHCupDir
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


upgradeOptsP :: Parser UpgradeOpts
upgradeOptsP =
  flag'
      UpgradeInplace
      (short 'i' <> long "inplace" <> help
        "Upgrade ghcup in-place"
      )
    <|>
      (   UpgradeAt
        <$> option
              str
              (short 't' <> long "target" <> metavar "TARGET_DIR" <> help
                "Absolute filepath to write ghcup into"
                <> completer (bashCompleter "file")
              )
        )
    <|> pure UpgradeGHCupDir




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type UpgradeEffects = '[ DigestError
                       , ContentLengthError
                       , GPGError
                       , NoDownload
                       , NoUpdate
                       , FileDoesNotExistError
                       , CopyError
                       , DownloadFailed
                       , ToolShadowed
                       , URIParseError
                       ]




    ------------------
    --[ Entrypoint ]--
    ------------------



upgrade :: ( Monad m
           , MonadMask m
           , MonadUnliftIO m
           , MonadFail m
           )
        => UpgradeOpts
        -> Bool
        -> Bool
        -> Dirs
        -> (IO (AppState, IO ()), LeanAppState)
        -> m ExitCode
upgrade uOpts force' fatal Dirs{..} (getAppState', leanAppstate) = do
  target <- case uOpts of
    UpgradeInplace  -> Just <$> liftIO getExecutablePath
    (UpgradeAt p)   -> pure $ Just p
    UpgradeGHCupDir -> pure (Just (binDir </> "ghcup" <> exeExt))

  run (do
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
    Just tver <- pure $ getLatest dls ghcup
    let latestVer = _tvVersion tver
    let vm = getVersionMetadata tver ghcup dls
    forM_ (_vmPreInstall =<< vm) $ \msg -> do
      lift $ logWarn msg
      lift $ logWarn
        "...waiting for 5 seconds, you can still abort..."
      liftIO $ threadDelay 5000000 -- give the user a sec to intervene
    v' <- liftE $ upgradeGHCup' target force' fatal latestVer
    pure (v', vm)
    ) >>= \case
      (VRight (v', vm), up) -> do
        let pretty_v = prettyVer v'
        runLogger $ logInfo $
          "Successfully upgraded GHCup to version " <> pretty_v
        forM_ (_vmPostInstall =<< vm) $ \msg ->
          runLogger $ logInfo msg
        liftIO up
        pure ExitSuccess
      (VLeft (V NoUpdate), _) -> do
        runLogger $ logWarn "No GHCup update available"
        pure ExitSuccess
      (VLeft e, _) -> do
        runLogger $ logError $ T.pack $ prettyHFError e
        pure $ ExitFailure 11
 where
  runLogger = flip runReaderT leanAppstate
  run action' = do
    (appstate', up) <- liftIO getAppState'
    r <- flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @UpgradeEffects
                  $ action'
    pure (r, up)
