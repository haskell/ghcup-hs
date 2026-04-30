{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.OptParse.Set where




import GHCup.OptParse.Common

import GHCup.Command.List
import GHCup.Command.Set
import GHCup.Errors
import GHCup.Input.Parsers
    ( SetToolVersion (..)
    , ghcVersionEither'
    , resolveVersion'
    , tagEither
    , toolParser
    , toolVersionEither'
    )
import GHCup.Prelude.Logger
import GHCup.Prelude.String.QQ
import GHCup.Types

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Exception.Safe          ( MonadMask )
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Bifunctor                  ( second )
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions
import GHC.Unicode
import Options.Applicative             hiding ( ParseError, style )
import Options.Applicative.Pretty.Shim ( text )
import Prelude                         hiding ( appendFile )
import System.Exit
import Text.PrettyPrint.HughesPJClass  ( prettyShow )

import qualified Data.Text as T




    ----------------
    --[ Commands ]--
    ----------------


data SetCommand
  = SetGHC SetOptions
  | SetCabal SetOptions
  | SetHLS SetOptions
  | SetStack SetOptions
  | SetOther SetOptionsNew
  deriving (Eq, Show)




    ---------------
    --[ Options ]--
    ---------------


data SetOptions = SetOptions
  { sToolVer :: SetToolVersion
  }
  deriving (Eq, Show)

data SetOptionsNew = SetOptionsNew
  { sTool :: Tool
  , sToolVer :: SetToolVersion
  }
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


setParser :: Parser SetCommand
setParser =
  subparser
      (  command
          "ghc"
          (   SetGHC
          <$> info
                (setOpts ghc <**> helper)
                (  progDesc "Set GHC version"
                <> footerDoc (Just $ text setGHCFooter)
                )
          )
      <> command
           "cabal"
           (   SetCabal
           <$> info
                 (setOpts cabal <**> helper)
                 (  progDesc "Set Cabal version"
                 <> footerDoc (Just $ text setCabalFooter)
                 )
           )
      <> command
           "hls"
           (   SetHLS
           <$> info
                 (setOpts hls <**> helper)
                 (  progDesc "Set haskell-language-server version"
                 <> footerDoc (Just $ text setHLSFooter)
                 )
           )
      <> command
           "stack"
           (   SetStack
           <$> info
                 (setOpts stack <**> helper)
                 (  progDesc "Set stack version"
                 <> footerDoc (Just $ text setStackFooter)
                 )
           )
      )
    <|> (   SetOther
           <$> setOptsNew
           )
 where
  setGHCFooter :: String
  setGHCFooter = [s|Discussion:
    Sets the the current GHC version by creating non-versioned
    symlinks for all ghc binaries of the specified version in
    "~/.ghcup/bin/<binary>".|]

  setCabalFooter :: String
  setCabalFooter = [s|Discussion:
    Sets the the current Cabal version.|]

  setStackFooter :: String
  setStackFooter = [s|Discussion:
    Sets the the current Stack version.|]

  setHLSFooter :: String
  setHLSFooter = [s|Discussion:
    Sets the the current haskell-language-server version.|]


setOpts :: Tool -> Parser SetOptions
setOpts tool =
    SetOptions . fromMaybe SetRecommended <$>
      optional (setVersionArgument [ListInstalled True] tool)

setOptsNew :: Parser SetOptionsNew
setOptsNew = SetOptionsNew
  <$> argument (eitherReader toolParser) (metavar "TOOL")
  <*> (fromMaybe SetRecommended <$> optional (argument (eitherReader setEither) (metavar "VERSION|TAG|next")))
 where
  setEither s' =
        parseSet s'
    <|> second SetToolTag (tagEither s')
    <|> second SetGHCVersion (ghcVersionEither' s')
  parseSet s' = case fmap toLower s' of
                  "next" -> Right SetNext
                  other  -> Left $ "Unknown tag/version " <> other

setVersionArgument :: [ListCriteria] -> Tool -> Parser SetToolVersion
setVersionArgument criteria tool =
  argument (eitherReader setEither)
    (metavar "VERSION|TAG|next"
    <> completer (tagCompleter tool ["next"])
    <> (completer . versionCompleter criteria) tool)
 where
  setEither s' =
        parseSet s'
    <|> second SetToolTag (tagEither s')
    <|> se s'
  se s' = case tool of
           Tool "ghc" -> second SetGHCVersion (ghcVersionEither' s')
           _          -> second SetToolVersion (toolVersionEither' s')
  parseSet s' = case fmap toLower s' of
                  "next" -> Right SetNext
                  other  -> Left $ "Unknown tag/version " <> other




    --------------
    --[ Footer ]--
    --------------


setFooter :: String
setFooter = [s|Discussion:
  Sets the currently active GHC or cabal version. When no command is given,
  defaults to setting GHC with the specified version/tag (if no tag
  is given, sets GHC to 'recommended' version).
  It is recommended to always specify a subcommand (ghc/cabal/hls/stack).|]



    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type SetEffects = '[ FileDoesNotExistError
                   , NotInstalled
                   , TagNotFound
                   , DayNotFound
                   , NextVerNotFound
                   , NoToolVersionSet
                   , ParseError
                   ]



    -------------------
    --[ Entrypoints ]--
    -------------------


set :: forall m. ( Monad m
       , MonadMask m
       , MonadUnliftIO m
       , MonadFail m
       )
    => SetCommand
    -> Settings
    -> (IO (AppState, IO ()), LeanAppState)
    -> m ExitCode
set setCommand settings (getAppState', leanAppstate) = case setCommand of
  (SetGHC sopts)   -> setOther (toSetOptionsNew ghc sopts)
  (SetCabal sopts) -> setOther (toSetOptionsNew cabal sopts)
  (SetHLS sopts)   -> setOther (toSetOptionsNew hls sopts)
  (SetStack sopts) -> setOther (toSetOptionsNew stack sopts)
  (SetOther sopts) -> setOther sopts

 where
  guessMode = if guessVersion settings then GLaxWithInstalled else GStrict
  runLogger = flip runReaderT leanAppstate
  run action' = do
    (appstate', up) <- liftIO getAppState'
    r <- flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @SetEffects
                  $ action'
    pure (r, up)


  toSetOptionsNew sTool SetOptions{..} = SetOptionsNew{..}

  setOther :: SetOptionsNew
           -> m ExitCode
  setOther SetOptionsNew{ sTool, sToolVer } = run (do
          (TargetVersionReq v _) <- liftE $ resolveVersion' sToolVer guessMode sTool
          liftE $ setToolVersion sTool v
        )
      >>= \case
            (VRight TargetVersion{..}, up) -> do
              runLogger
                $ logInfo $
                    T.pack (prettyShow sTool) <> " " <> prettyVer _tvVersion <> " successfully set as default version" <> maybe "" (" for cross target " <>) _tvTarget
              liftIO up
              pure ExitSuccess
            (VLeft e, _) -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 5


