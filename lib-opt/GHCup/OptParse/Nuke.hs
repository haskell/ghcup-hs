{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Nuke where




import           GHCup.Command.List (ListResult(..), ListCriteria(..), listVersions)
import           GHCup.Command.Rm
import           GHCup.Command.Nuke
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Prelude.Logger

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad (forM_, void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Foldable.WithIndex
import           Data.Maybe
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import Control.DeepSeq
import Control.Exception
import Control.Concurrent (threadDelay)




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type NukeEffects = '[ NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo ]


runNuke :: AppState
        -> Excepts NukeEffects (ReaderT AppState m) a
        -> m (VEither NukeEffects a)
runNuke s' =
  flip runReaderT s' . runE @NukeEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



nuke :: ( Monad m
        , MonadMask m
        , MonadUnliftIO m
        , MonadFail m
        )
     => IO AppState
     -> (ReaderT LeanAppState m () -> m ())
     -> m ExitCode
nuke appState runLogger = do
  s' <- liftIO appState
  void $ liftIO $ evaluate $ force s'
  runNuke s' (do
       lift $ logWarn "WARNING: This will remove GHCup and all installed components from your system."
       lift $ logWarn "Waiting 10 seconds before commencing, if you want to cancel it, now would be the time."
       liftIO $ threadDelay 10000000  -- wait 10s

       lift $ logInfo "Initiating Nuclear Sequence 🚀🚀🚀"
       lift $ logInfo "Nuking in 3...2...1"

       lInstalled' <- liftE $ listVersions Nothing [ListInstalled True] False True (Nothing, Nothing)

       iforM_ lInstalled' $ \tool (_, ls) -> forM_ ls $ \ListResult{..} -> liftE $ rmToolVersion tool (TargetVersion lCross lVer)

       lift rmGhcupDirs

       ) >>= \case
                VRight leftOverFiles
                  | null leftOverFiles -> do
                      runLogger $ logInfo "Nuclear Annihilation complete!"
                      pure ExitSuccess
                  | otherwise -> do
                      runLogger $ logError "These Files have survived Nuclear Annihilation, you may remove them manually."
                      liftIO $ forM_ leftOverFiles putStrLn
                      pure ExitSuccess

                VLeft e -> do
                  runLogger $ logError $ T.pack $ prettyHFError e
                  pure $ ExitFailure 15
