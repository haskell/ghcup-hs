{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.UnSet where




import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import GHCup.Types.Optics




    ----------------
    --[ Commands ]--
    ----------------


data UnsetCommand = UnsetGHC   UnsetOptions
                  | UnsetCabal UnsetOptions
                  | UnsetHLS   UnsetOptions
                  | UnsetStack UnsetOptions




    ---------------
    --[ Options ]--
    ---------------


data UnsetOptions = UnsetOptions
  { sToolVer :: Maybe T.Text -- target platform triple
  }




    ---------------
    --[ Parsers ]--
    ---------------

          
unsetParser :: Parser UnsetCommand
unsetParser =
  subparser
      (  command
          "ghc"
          (   UnsetGHC
          <$> info
                (unsetOpts <**> helper)
                (  progDesc "Unset GHC version"
                <> footerDoc (Just $ text unsetGHCFooter)
                )
          )
      <> command
           "cabal"
           (   UnsetCabal
           <$> info
                (unsetOpts <**> helper)
                 (  progDesc "Unset Cabal version"
                 <> footerDoc (Just $ text unsetCabalFooter)
                 )
           )
      <> command
           "hls"
           (   UnsetHLS
           <$> info
                 (unsetOpts <**> helper)
                 (  progDesc "Unset haskell-language-server version"
                 <> footerDoc (Just $ text unsetHLSFooter)
                 )
           )
      <> command
           "stack"
           (   UnsetStack
           <$> info
                 (unsetOpts <**> helper)
                 (  progDesc "Unset stack version"
                 <> footerDoc (Just $ text unsetStackFooter)
                 )
           )
      )
 where
  unsetGHCFooter :: String
  unsetGHCFooter = [s|Discussion:
    Unsets the the current GHC version. That means there won't
    be a ~/.ghcup/bin/ghc anymore.|]

  unsetCabalFooter :: String
  unsetCabalFooter = [s|Discussion:
    Unsets the the current Cabal version.|]

  unsetStackFooter :: String
  unsetStackFooter = [s|Discussion:
    Unsets the the current Stack version.|]

  unsetHLSFooter :: String
  unsetHLSFooter = [s|Discussion:
    Unsets the the current haskell-language-server version.|]


unsetOpts :: Parser UnsetOptions
unsetOpts = UnsetOptions . fmap T.pack <$> optional (argument str (metavar "TRIPLE"))



    --------------
    --[ Footer ]--
    --------------


unsetFooter :: String
unsetFooter = [s|Discussion:
  Unsets the currently active GHC or cabal version.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type UnsetEffects = '[ NotInstalled ]


runUnsetGHC :: (ReaderT env m (VEither UnsetEffects a) -> m (VEither UnsetEffects a))
            -> Excepts UnsetEffects (ReaderT env m) a
            -> m (VEither UnsetEffects a)
runUnsetGHC runLeanAppState =
    runLeanAppState
    . runE
      @UnsetEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



unset :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         , HasDirs env
         , HasLog env
         )
      => UnsetCommand
      -> (ReaderT env m (VEither UnsetEffects ())
          -> m (VEither UnsetEffects ()))
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
unset unsetCommand runLeanAppState runLogger = case unsetCommand of
  (UnsetGHC (UnsetOptions triple)) -> runUnsetGHC runLeanAppState (unsetGHC triple)
        >>= \case
              VRight _ -> do
                runLogger $ logInfo "GHC successfully unset"
                pure ExitSuccess
              VLeft  e -> do
                runLogger $ logError $ T.pack $ prettyHFError e
                pure $ ExitFailure 14
  (UnsetCabal (UnsetOptions _)) -> do
    void $ runLeanAppState (VRight <$> unsetCabal)
    runLogger $ logInfo "Cabal successfully unset"
    pure ExitSuccess
  (UnsetHLS (UnsetOptions _)) -> do
    void $ runLeanAppState (VRight <$> unsetHLS)
    runLogger $ logInfo "HLS successfully unset"
    pure ExitSuccess
  (UnsetStack (UnsetOptions _)) -> do
    void $ runLeanAppState (VRight <$> unsetStack)
    runLogger $ logInfo "Stack successfully unset"
    pure ExitSuccess
