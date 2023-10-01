{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCup.OptParse.ToolRequirements where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import Control.Exception.Safe (MonadMask)
import GHCup.Types.Optics
import GHCup.Platform
import GHCup.Prelude
import GHCup.Requirements
import System.IO



    ---------------
    --[ Options ]--
    ---------------


data ToolReqOpts = ToolReqOpts
  { tlrRaw :: Bool
  }




    ---------------
    --[ Parsers ]--
    ---------------

          
toolReqP :: Parser ToolReqOpts
toolReqP =
  ToolReqOpts
    <$> switch (short 'r' <> long "raw-format" <> help "machine-parsable format")




    --------------
    --[ Footer ]--
    --------------


toolReqFooter :: String
toolReqFooter = [s|Discussion:
  Print tool requirements on the current platform.
  If you want to pass this to your package manage, use '--raw-format'.|]



    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type ToolRequirementsEffects = '[ NoCompatiblePlatform , DistroNotFound , NoToolRequirements ]


runToolRequirements :: (ReaderT env m (VEither ToolRequirementsEffects a) -> m (VEither ToolRequirementsEffects a))
                    -> Excepts ToolRequirementsEffects (ReaderT env m) a
                    -> m (VEither ToolRequirementsEffects a)
runToolRequirements runAppState =
    runAppState
    . runE
      @ToolRequirementsEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



toolRequirements :: ( Monad m
                    , MonadMask m
                    , MonadUnliftIO m
                    , MonadFail m
                    , Alternative m
                    )
                 => ToolReqOpts
                 -> (ReaderT AppState m (VEither ToolRequirementsEffects ()) -> m (VEither ToolRequirementsEffects ()))
                 -> (ReaderT LeanAppState m () -> m ())
                 -> m ExitCode
toolRequirements ToolReqOpts{..} runAppState runLogger = runToolRequirements runAppState (do
    GHCupInfo { .. } <- lift getGHCupInfo
    platform' <- liftE getPlatform
    req       <- getCommonRequirements platform' _toolRequirements ?? NoToolRequirements
    if tlrRaw
    then liftIO $ T.hPutStr stdout (rawRequirements req)
    else liftIO $ T.hPutStr stdout (prettyRequirements req)
  )
    >>= \case
          VRight _ -> pure ExitSuccess
          VLeft  e -> do
            runLogger $ logError $ T.pack $ prettyHFError e
            pure $ ExitFailure 12
