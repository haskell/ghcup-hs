{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.DInfo where




import           GHCup
import           GHCup.Errors
import           GHCup.Version
import           GHCup.Types
import           GHCup.Utils.Dirs
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process

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
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import Language.Haskell.TH



    -----------------
    --[ Utilities ]--
    -----------------


describe_result :: String
describe_result = $( LitE . StringL <$>
                     runIO (do
                             CapturedProcess{..} <-  do
                              dirs <- liftIO getAllDirs
                              let settings = AppState (defaultSettings { noNetwork = True })
                                               dirs
                                               defaultKeyBindings
                              flip runReaderT settings $ executeOut "git" ["describe"] Nothing
                             case _exitCode of
                               ExitSuccess   -> pure . T.unpack . decUTF8Safe' $ _stdOut
                               ExitFailure _ -> pure numericVer
                     )
                   )


prettyDebugInfo :: DebugInfo -> String
prettyDebugInfo DebugInfo {..} = "Debug Info" <> "\n" <>
  "==========" <> "\n" <>
  "GHCup base dir: " <> diBaseDir <> "\n" <>
  "GHCup bin dir: " <> diBinDir <> "\n" <>
  "GHCup GHC directory: " <> diGHCDir <> "\n" <>
  "GHCup cache directory: " <> diCacheDir <> "\n" <>
  "Architecture: " <> prettyShow diArch <> "\n" <>
  "Platform: " <> prettyShow diPlatform <> "\n" <>
  "Version: " <> describe_result



    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type DInfoEffects = '[ NoCompatiblePlatform , NoCompatibleArch , DistroNotFound ]

runDebugInfo :: (ReaderT env m (VEither DInfoEffects a) -> m (VEither DInfoEffects a))
             -> Excepts DInfoEffects (ReaderT env m) a
             -> m (VEither DInfoEffects a)
runDebugInfo runAppState =
        runAppState
        . runE
          @DInfoEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



dinfo :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         , Alternative m
         )
      => (ReaderT AppState m (VEither DInfoEffects DebugInfo)
          -> m (VEither DInfoEffects DebugInfo))
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
dinfo runAppState runLogger = do
  runDebugInfo runAppState (liftE getDebugInfo)
    >>= \case
          VRight di -> do
            liftIO $ putStrLn $ prettyDebugInfo di
            pure ExitSuccess
          VLeft e -> do
            runLogger $ logError $ T.pack $ prettyHFError e
            pure $ ExitFailure 8
