{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.HealthCheck where


import GHCup.OptParse.Common

import GHCup.Command.HealthCheck
import GHCup.Errors
import GHCup.Input.Parsers
    ( toolParser )
import GHCup.Prelude.Logger
import GHCup.Prelude.String.QQ
import GHCup.Types

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import Options.Applicative.Pretty.Shim ( text )
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Control.Exception.Safe (MonadMask)

import qualified Data.Text                     as T





    ----------------
    --[ Commands ]--
    ----------------


data HealthCheckCommand
  = HealthCheckTool ToolOptions
  deriving (Eq, Show)




    ---------------
    --[ Options ]--
    ---------------


data ToolOptions = ToolOptions
  { checkTool :: Tool
  , checkVer :: TargetVersion
  }
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


hcP :: Parser HealthCheckCommand
hcP =
  subparser
      (  command
          "tool"
          (   HealthCheckTool
          <$> info
                (toolOpts <**> helper)
                (  progDesc "Check tool for problems"
                <> footerDoc (Just $ text toolFooter)
                )
          )
      )
 where
  toolFooter :: String
  toolFooter = [s|Discussion:
  Check a tool for problems.

Examples:
  ghcup check tool |]

toolOpts :: Parser ToolOptions
toolOpts =
  (\t v -> ToolOptions t v)
    <$> argument (eitherReader toolParser) (metavar "TOOL" <> help "Which tool to install")
    <*> ghcVersionArgument [] Nothing


    --------------
    --[ Footer ]--
    --------------


hcFooter :: String
hcFooter = [s|Discussion:
  Performs various health checks. Good for attaching to bug reports.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type HCEffects = '[ ]




    ------------------
    --[ Entrypoint ]--
    ------------------



hc :: ( Monad m
      , MonadMask m
      , MonadUnliftIO m
      , MonadFail m
      )
   => HealthCheckCommand
   -> (IO (AppState, IO ()), LeanAppState)
   -> m ExitCode
hc command' (getAppState', leanAppstate) = run (do
     case command' of
       HealthCheckTool ToolOptions{..} ->
         lift $ dbHealthCheck checkTool checkVer
   ) >>= \case
            (VRight r, up) -> do
              liftIO $ putStr $ prettyShow r
              liftIO up
              pure ExitSuccess
            (VLeft e, _) -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 27
 where
  runLogger = flip runReaderT leanAppstate
  run action' = do
    (appstate', up) <- liftIO getAppState'
    r <- flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @HCEffects
                  $ action'
    pure (r, up)

