{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Upgrade where




import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger

import           Control.Concurrent (threadDelay)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad (forM_)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import System.Environment
import GHCup.Utils
import System.FilePath
import GHCup.Types.Optics
import Data.Versions






    ---------------
    --[ Options ]--
    ---------------


data UpgradeOpts = UpgradeInplace
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
                       ]


runUpgrade :: MonadUnliftIO m
           => (ReaderT AppState m (VEither UpgradeEffects a) -> m (VEither UpgradeEffects a))
           -> Excepts UpgradeEffects (ResourceT (ReaderT AppState m)) a
           -> m (VEither UpgradeEffects a)
runUpgrade runAppState =
  runAppState
  . runResourceT
  . runE
    @UpgradeEffects



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
        -> (forall a. ReaderT AppState m (VEither UpgradeEffects a) -> m (VEither UpgradeEffects a))
        -> (ReaderT LeanAppState m () -> m ())
        -> m ExitCode
upgrade uOpts force' fatal Dirs{..} runAppState runLogger = do
  target <- case uOpts of
    UpgradeInplace  -> Just <$> liftIO getExecutablePath
    (UpgradeAt p)   -> pure $ Just p
    UpgradeGHCupDir -> pure (Just (binDir </> "ghcup" <> exeExt))

  runUpgrade runAppState (do
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
    Just (tver, vi) <- pure $ getLatest dls GHCup
    let latestVer = _tvVersion tver
    forM_ (_viPreInstall vi) $ \msg -> do
      lift $ logWarn msg
      lift $ logWarn
        "...waiting for 5 seconds, you can still abort..."
      liftIO $ threadDelay 5000000 -- give the user a sec to intervene
    v' <- liftE $ upgradeGHCup' target force' fatal latestVer
    pure (v', dls)
    ) >>= \case
      VRight (v', dls) -> do
        let pretty_v = prettyVer v'
        let vi = fromJust $ snd <$> getLatest dls GHCup
        runLogger $ logInfo $
          "Successfully upgraded GHCup to version " <> pretty_v
        forM_ (_viPostInstall vi) $ \msg ->
          runLogger $ logInfo msg
        pure ExitSuccess
      VLeft (V NoUpdate) -> do
        runLogger $ logWarn "No GHCup update available"
        pure ExitSuccess
      VLeft e -> do
        runLogger $ logError $ T.pack $ prettyHFError e
        pure $ ExitFailure 11
