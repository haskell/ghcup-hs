{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module GHCup.OptParse.Rm where




import GHCup.Command.List
import GHCup.Command.Rm
import GHCup.Errors
import GHCup.Input.Parsers     ( ghcVersionEither, toolParser )
import GHCup.OptParse.Common
import GHCup.Prelude.Logger
import GHCup.Prelude.String.QQ
import GHCup.Query.Metadata
import GHCup.Types
import GHCup.Types.Optics

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad                  ( forM_, when )
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions
import Options.Applicative            hiding ( ParseError, style )
import Prelude                        hiding ( appendFile )
import System.Exit
import Text.PrettyPrint.HughesPJClass ( prettyShow )

import           Control.Exception.Safe ( MonadMask )
import qualified Data.Text              as T




    ----------------
    --[ Commands ]--
    ----------------


data RmCommand
  = RmGHC RmOptions
  | RmCabal Version
  | RmHLS Version
  | RmStack Version
  | RmOther RmOptionsNew
  deriving (Eq, Show)




    ---------------
    --[ Options ]--
    ---------------


data RmOptions = RmOptions
  { ghcVer :: TargetVersion
  }
  deriving (Eq, Show)

data RmOptionsNew = RmOptionsNew
  { rmTool :: Tool
  , ghcVer :: TargetVersion
  }
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


rmParser :: Parser RmCommand
rmParser =
  subparser
      (  command
          "ghc"
          (RmGHC <$> info (rmOpts (Just ghc) <**> helper) (progDesc "Remove GHC version"))
      <> command
           "cabal"
           (   RmCabal
           <$> info (versionParser' [ListInstalled True] (Just cabal) <**> helper)
                    (progDesc "Remove Cabal version")
           )
      <> command
           "hls"
           (   RmHLS
           <$> info (versionParser' [ListInstalled True] (Just hls) <**> helper)
                    (progDesc "Remove haskell-language-server version")
           )
      <> command
           "stack"
           (   RmStack
           <$> info (versionParser' [ListInstalled True] (Just stack) <**> helper)
                    (progDesc "Remove stack version")
           )
      )
    <|> (   RmOther
           <$> rmOptsNew
           )



rmOpts :: Maybe Tool -> Parser RmOptions
rmOpts tool = RmOptions <$> ghcVersionArgument [ListInstalled True] tool

rmOptsNew :: Parser RmOptionsNew
rmOptsNew = RmOptionsNew
  <$> argument (eitherReader toolParser) (metavar "TOOL")
  <*> argument (eitherReader ghcVersionEither) (metavar "VERSION")





    --------------
    --[ Footer ]--
    --------------


rmFooter :: String
rmFooter = [s|Discussion:
  Remove the given GHC or cabal version. When no command is given,
  defaults to removing GHC with the specified version.
  It is recommended to always specify a subcommand (ghc/cabal/hls/stack).|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type RmEffects = '[ NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo ]


runRm :: (ReaderT env m (VEither RmEffects a) -> m (VEither RmEffects a))
            -> Excepts RmEffects (ReaderT env m) a
            -> m (VEither RmEffects a)
runRm runAppState =
    runAppState
    . runE
      @RmEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



rm :: ( Monad m
      , MonadMask m
      , MonadUnliftIO m
      , MonadFail m
      )
   => RmCommand
   -> (ReaderT AppState m (VEither RmEffects (Maybe VersionInfo))
       -> m (VEither RmEffects (Maybe VersionInfo)))
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
rm rmCommand runAppState runLogger = case rmCommand of
  (RmGHC rmopts) -> rmOther (toRmOptionsNew ghc rmopts)
  (RmCabal (RmOptions . mkTVer -> rmopts)) -> rmOther (toRmOptionsNew cabal rmopts)
  (RmHLS (RmOptions . mkTVer -> rmopts)) -> rmOther (toRmOptionsNew hls rmopts)
  (RmStack (RmOptions . mkTVer -> rmopts)) -> rmOther (toRmOptionsNew stack rmopts)
  (RmOther rmopts) -> rmOther rmopts

 where
  toRmOptionsNew rmTool RmOptions{..} = RmOptionsNew{..}

  rmOther RmOptionsNew{..} =
    runRm runAppState (do
        liftE $
          rmToolVersion rmTool ghcVer
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        pure (getVersionInfo ghcVer rmTool dls)
      )
      >>= \case
            VRight vi -> do
              postRmLog (tVerToText ghcVer) rmTool vi
              when (rmTool == ghc) $ runLogger $ logGHCPostRm ghcVer
              pure ExitSuccess
            VLeft  e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 7

  postRmLog tv tool vi = runLogger $ do
    logInfo $ "Successfully removed " <> T.pack (prettyShow tool) <> " " <> tv
    forM_ (_viPostRemove =<< vi) logInfo
