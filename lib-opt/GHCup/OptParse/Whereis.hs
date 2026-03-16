{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.OptParse.Whereis where




import GHCup.Command.Whereis
import GHCup.Errors
import GHCup.Input.Parsers     ( fromVersion, toolParser )
import GHCup.OptParse.Common
import GHCup.Prelude.Logger
import GHCup.Prelude.String.QQ
import GHCup.Types

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Options.Applicative             hiding ( ParseError, style )
import Options.Applicative.Pretty.Shim ( text )
import Prelude                         hiding ( appendFile )
import System.Environment
import System.Exit
import System.FilePath

import           Control.Exception.Safe ( MonadMask )
import qualified Data.Text              as T
import           GHCup.Query.GHCupDirs
import           GHCup.System.Directory
import           GHCup.Types.Optics




    ----------------
    --[ Commands ]--
    ----------------


data WhereisCommand
  = WhereisTool Tool (Maybe ToolVersion)
  | WhereisBaseDir
  | WhereisBinDir
  | WhereisCacheDir
  | WhereisLogsDir
  | WhereisConfDir
  deriving (Eq, Show)





    ---------------
    --[ Options ]--
    ---------------


data WhereisOptions = WhereisOptions
  { directory :: Bool
  }
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


whereisP :: Parser WhereisCommand
whereisP = subparser
  (commandGroup "Tools locations:" <>
    command
      "ghc"
      (WhereisTool ghc <$> info
        ( optional (toolVersionTagArgument [] (Just ghc)) <**> helper )
        ( progDesc "Get GHC location"
        <> footerDoc (Just $ text whereisGHCFooter ))
      )
      <>
     command
      "cabal"
      (WhereisTool cabal <$> info
        ( optional (toolVersionTagArgument [] (Just cabal)) <**> helper )
        ( progDesc "Get cabal location"
        <> footerDoc (Just $ text whereisCabalFooter ))
      )
      <>
     command
      "hls"
      (WhereisTool hls <$> info
        ( optional (toolVersionTagArgument [] (Just hls)) <**> helper )
        ( progDesc "Get HLS location"
        <> footerDoc (Just $ text whereisHLSFooter ))
      )
      <>
     command
      "stack"
      (WhereisTool stack <$> info
        ( optional (toolVersionTagArgument [] (Just stack)) <**> helper )
        ( progDesc "Get stack location"
        <> footerDoc (Just $ text whereisStackFooter ))
      )
      <>
     command
      "ghcup"
      (WhereisTool ghcup <$> info ( pure Nothing <**> helper ) ( progDesc "Get ghcup location" ))
    )
    <|> subparser ( commandGroup "Directory locations:"
      <>
     command
      "basedir"
      (info (pure WhereisBaseDir <**> helper)
            ( progDesc "Get ghcup base directory location" )
      )
      <>
     command
      "bindir"
      (info (pure WhereisBinDir <**> helper)
            ( progDesc "Get ghcup binary directory location" )
      )
      <>
     command
      "cachedir"
      (info (pure WhereisCacheDir <**> helper)
            ( progDesc "Get ghcup cache directory location" )
      )
      <>
     command
      "logsdir"
      (info (pure WhereisLogsDir <**> helper)
            ( progDesc "Get ghcup logs directory location" )
      )
      <>
     command
      "confdir"
      (info (pure WhereisConfDir <**> helper)
            ( progDesc "Get ghcup config directory location" )
      )
  )
    <|>
    (
     WhereisTool <$> argument (eitherReader toolParser) (metavar "TOOL")
                 <*> optional (toolVersionTagArgument [] Nothing)
    )
 where
  whereisGHCFooter = [s|Discussion:
  Finds the location of a GHC executable, which usually resides in
  a self-contained "~/.ghcup/ghc/<ghcver>" directory.

Examples:
  # outputs ~/.ghcup/ghc/8.10.5/bin/ghc.exe
  ghcup whereis ghc 8.10.5
  # outputs ~/.ghcup/ghc/8.10.5/bin/
  ghcup whereis --directory ghc 8.10.5 |]

  whereisCabalFooter = [s|Discussion:
  Finds the location of a Cabal executable, which usually resides in
  "~/.ghcup/bin/".

Examples:
  # outputs ~/.ghcup/bin/cabal-3.4.0.0
  ghcup whereis cabal 3.4.0.0
  # outputs ~/.ghcup/bin
  ghcup whereis --directory cabal 3.4.0.0|]

  whereisHLSFooter = [s|Discussion:
  Finds the location of a HLS executable, which usually resides in
  "~/.ghcup/bin/".

Examples:
  # outputs ~/.ghcup/bin/haskell-language-server-wrapper-1.2.0
  ghcup whereis hls 1.2.0
  # outputs ~/.ghcup/bin/
  ghcup whereis --directory hls 1.2.0|]

  whereisStackFooter = [s|Discussion:
  Finds the location of a stack executable, which usually resides in
  "~/.ghcup/bin/".

Examples:
  # outputs ~/.ghcup/bin/stack-2.7.1
  ghcup whereis stack 2.7.1
  # outputs ~/.ghcup/bin/
  ghcup whereis --directory stack 2.7.1|]



    --------------
    --[ Footer ]--
    --------------


whereisFooter :: String
whereisFooter = [s|Discussion:
  Finds the location of a tool. For GHC, this is the ghc binary, that
  usually resides in a self-contained "~/.ghcup/ghc/<ghcver>" directory.
  For cabal/stack/hls this the binary usually at "~/.ghcup/bin/<tool>-<ver>".

Examples:
  # outputs ~/.ghcup/ghc/8.10.5/bin/ghc.exe
  ghcup whereis ghc 8.10.5
  # outputs ~/.ghcup/ghc/8.10.5/bin/
  ghcup whereis --directory ghc 8.10.5
  # outputs ~/.ghcup/bin/cabal-3.4.0.0
  ghcup whereis cabal 3.4.0.0
  # outputs ~/.ghcup/bin/
  ghcup whereis --directory cabal 3.4.0.0|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type WhereisEffects = '[ NotInstalled
                , NoToolVersionSet
                , NextVerNotFound
                , TagNotFound
                , DayNotFound
                , ParseError
                , NoInstallInfo
                ]


runLeanWhereIs :: (MonadUnliftIO m, MonadIO m)
               => LeanAppState
               -> Excepts WhereisEffects (ReaderT LeanAppState m) a
               -> m (VEither WhereisEffects a)
runLeanWhereIs leanAppstate =
    -- Don't use runLeanAppState here, which is disabled on windows.
    -- This is the only command on all platforms that doesn't need full appstate.
    flip runReaderT leanAppstate
    . runE
      @WhereisEffects


runWhereIs :: (MonadUnliftIO m, MonadIO m)
           => (ReaderT AppState m (VEither WhereisEffects a) -> m (VEither WhereisEffects a))
           -> Excepts WhereisEffects (ReaderT AppState m) a
           -> m (VEither WhereisEffects a)
runWhereIs runAppState =
    runAppState
    . runE
      @WhereisEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



whereis :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         )
      => WhereisCommand
      -> WhereisOptions
      -> Settings
      -> (forall a. ReaderT AppState m (VEither WhereisEffects a) -> m (VEither WhereisEffects a))
      -> LeanAppState
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
whereis whereisCommand whereisOptions settings runAppState leanAppstate runLogger = do
  Dirs{ .. }  <- runReaderT getDirs leanAppstate
  case (whereisCommand, whereisOptions) of
    (WhereisTool (Tool "ghcup") _, WhereisOptions{..}) -> do
      loc <- liftIO (getExecutablePath >>= canon )
      if directory
      then liftIO $ putStr $ takeDirectory loc
      else liftIO $ putStr loc
      pure ExitSuccess

    (WhereisTool tool (Just (GHCVersion v)), WhereisOptions{..}) ->
      runLeanWhereIs leanAppstate (do
        loc <- liftE $ whereIsTool tool v
        if directory
        then takeDirectory <$> canon loc
        else canon loc
        )
        >>= \case
              VRight r -> do
                liftIO $ putStr r
                pure ExitSuccess
              VLeft e -> do
                runLogger $ logError $ T.pack $ prettyHFError e
                pure $ ExitFailure 30
    (WhereisTool tool (Just (ToolVersion v)), WhereisOptions{..}) ->
      runLeanWhereIs leanAppstate (do
        loc <- liftE $ whereIsTool tool (mkTVer v)
        if directory
        then takeDirectory <$> canon loc
        else canon loc
        )
        >>= \case
              VRight r -> do
                liftIO $ putStr r
                pure ExitSuccess
              VLeft e -> do
                runLogger $ logError $ T.pack $ prettyHFError e
                pure $ ExitFailure 30

    (WhereisTool tool whereVer, WhereisOptions{..}) -> do
      runWhereIs runAppState (do
        (v, _) <- liftE $ fromVersion whereVer guessMode tool
        loc <- liftE $ whereIsTool tool v
        if directory
        then takeDirectory <$> canon loc
        else canon loc
        )
        >>= \case
              VRight r -> do
                liftIO $ putStr r
                pure ExitSuccess
              VLeft e -> do
                runLogger $ logError $ T.pack $ prettyHFError e
                pure $ ExitFailure 30

    (WhereisBaseDir, _) -> do
      liftIO $ putStr =<< canon (fromGHCupPath baseDir)
      pure ExitSuccess

    (WhereisBinDir, _) -> do
      liftIO $ putStr =<< canon binDir
      pure ExitSuccess

    (WhereisCacheDir, _) -> do
      liftIO $ putStr =<< canon (fromGHCupPath cacheDir)
      pure ExitSuccess

    (WhereisLogsDir, _) -> do
      liftIO $ putStr =<< canon (fromGHCupPath logsDir)
      pure ExitSuccess

    (WhereisConfDir, _) -> do
      liftIO $ putStr =<< canon (fromGHCupPath confDir)
      pure ExitSuccess
 where
  -- make sure we only have forward slashes on windows
  canon fp = do
    cfp <- liftIO $ canonicalizePath fp
    pure $ map (\c -> if isPathSeparator c then '/' else c) cfp

  guessMode = if guessVersion settings then GLaxWithInstalled else GStrict

