{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Set where




import           GHCup.OptParse.Common

import           GHCup.Errors
import           GHCup.Command.List
import           GHCup.Command.Set
import           GHCup.Types
import           GHCup.Input.Parsers (SetToolVersion(..), tagEither, ghcVersionEither, toolVersionEither, fromVersion', toolParser)
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           Data.Versions
import           GHC.Unicode
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
import           Options.Applicative.Pretty.Shim ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Data.Bifunctor (second)
import Control.Exception.Safe (MonadMask)
import GHCup.Types.Optics
import Text.PrettyPrint.HughesPJClass (prettyShow)




    ----------------
    --[ Commands ]--
    ----------------


data SetCommand = SetGHC SetOptions
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
  } deriving (Eq, Show)

data SetOptionsNew = SetOptionsNew
  { sTool    :: Tool
  , sToolVer :: SetToolVersion
  } deriving (Eq, Show)




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
    <|> second SetGHCVersion (ghcVersionEither s')
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
           Tool "ghc" -> second SetGHCVersion (ghcVersionEither s')
           _   -> second SetToolVersion (toolVersionEither s')
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


type SetGHCEffects = '[ FileDoesNotExistError
                   , NotInstalled
                   , TagNotFound
                   , DayNotFound
                   , NextVerNotFound
                   , NoToolVersionSet
                   , ParseError
                   ]

runSetGHC :: (ReaderT env m (VEither SetGHCEffects a) -> m (VEither SetGHCEffects a))
          -> Excepts SetGHCEffects (ReaderT env m) a
          -> m (VEither SetGHCEffects a)
runSetGHC runAppState =
    runAppState
    . runE
      @SetGHCEffects


type SetCabalEffects = '[ NotInstalled
                        , TagNotFound
                        , DayNotFound
                        , NextVerNotFound
                        , NoToolVersionSet
                        , ParseError
                        ]

runSetCabal :: (ReaderT env m (VEither SetCabalEffects a) -> m (VEither SetCabalEffects a))
            -> Excepts SetCabalEffects (ReaderT env m) a
            -> m (VEither SetCabalEffects a)
runSetCabal runAppState =
    runAppState
    . runE
      @SetCabalEffects


type SetHLSEffects = '[ NotInstalled
                      , TagNotFound
                      , DayNotFound
                      , NextVerNotFound
                      , NoToolVersionSet]

runSetHLS :: (ReaderT env m (VEither SetHLSEffects a) -> m (VEither SetHLSEffects a))
          -> Excepts SetHLSEffects (ReaderT env m) a
          -> m (VEither SetHLSEffects a)
runSetHLS runAppState =
    runAppState
    . runE
      @SetHLSEffects


type SetStackEffects = '[ NotInstalled
                        , TagNotFound
                        , DayNotFound
                        , NextVerNotFound
                        , NoToolVersionSet]

runSetStack :: (ReaderT env m (VEither SetStackEffects a) -> m (VEither SetStackEffects a))
            -> Excepts SetStackEffects (ReaderT env m) a
            -> m (VEither SetStackEffects a)
runSetStack runAppState =
    runAppState
    . runE
      @SetStackEffects



    -------------------
    --[ Entrypoints ]--
    -------------------


set :: forall m env.
       ( Monad m
       , MonadMask m
       , MonadUnliftIO m
       , MonadFail m
       , HasDirs env
       , HasLog env
       )
    => SetCommand
    -> Settings
    -> (forall eff . ReaderT AppState m (VEither eff GHCTargetVersion)
        -> m (VEither eff GHCTargetVersion))
    -> (forall eff. ReaderT env m (VEither eff GHCTargetVersion)
        -> m (VEither eff GHCTargetVersion))
    -> (ReaderT LeanAppState m () -> m ())
    -> m ExitCode
set setCommand settings runAppState _ runLogger = case setCommand of
  (SetGHC sopts)   -> setOther (toSetOptionsNew ghc sopts)
  (SetCabal sopts) -> setOther (toSetOptionsNew cabal sopts)
  (SetHLS sopts)   -> setOther (toSetOptionsNew hls sopts)
  (SetStack sopts) -> setOther (toSetOptionsNew stack sopts)
  (SetOther sopts) -> setOther sopts

 where
  guessMode = if guessVersion settings then GLaxWithInstalled else GStrict

  toSetOptionsNew sTool SetOptions{..} = SetOptionsNew{..}

  setOther :: SetOptionsNew
           -> m ExitCode
  setOther SetOptionsNew{ sTool, sToolVer } = runSetGHC runAppState (do
          v <- liftE $ fst <$> fromVersion' sToolVer guessMode sTool
          liftE $ setToolVersion sTool v
        )
      >>= \case
            VRight GHCTargetVersion{..} -> do
              runLogger
                $ logInfo $
                    T.pack (prettyShow sTool) <> " " <> prettyVer _tvVersion <> " successfully set as default version" <> maybe "" (" for cross target " <>) _tvTarget
              pure ExitSuccess
            VLeft e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 5


