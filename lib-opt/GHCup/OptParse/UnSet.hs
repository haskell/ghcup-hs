{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.UnSet where




import           GHCup.Errors
import           GHCup.Input.Parsers (toolParser)
import           GHCup.Command.Set
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
import           Options.Applicative.Pretty.Shim ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import           Control.Exception.Safe (MonadMask)

import           Text.PrettyPrint.HughesPJClass (prettyShow)




    ----------------
    --[ Commands ]--
    ----------------


data UnsetCommand = UnsetGHC   UnsetOptions
                  | UnsetCabal UnsetOptions
                  | UnsetHLS   UnsetOptions
                  | UnsetStack UnsetOptions
                  | UnsetOther UnsetOptionsNew
                  deriving (Eq, Show)




    ---------------
    --[ Options ]--
    ---------------


data UnsetOptions = UnsetOptions
  { sToolTriple :: Maybe T.Text -- target platform triple
  } deriving (Eq, Show)

data UnsetOptionsNew = UnsetOptionsNew
  { sTool :: Tool
  , sToolTriple :: Maybe T.Text -- target platform triple
  } deriving (Eq, Show)



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
      ) <|>
      ( UnsetOther <$> unsetOptsNew
      )
 where
  unsetGHCFooter :: String
  unsetGHCFooter = [s|Discussion:
    Unsets the the current GHC version. That means there won't
    be a ~/.ghcup/bin/ghc anymore.

Examples:
  # unset ghc
  ghcup unset ghc

  # unset ghc for the target version
  ghcup unset ghc armv7-unknown-linux-gnueabihf|]

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

unsetOptsNew :: Parser UnsetOptionsNew
unsetOptsNew = UnsetOptionsNew
  <$> argument (eitherReader toolParser) (metavar "TOOL")
  <*> (fmap T.pack <$> optional (argument str (metavar "TRIPLE")))



    --------------
    --[ Footer ]--
    --------------


unsetFooter :: String
unsetFooter = [s|Discussion:
  Unsets the currently active GHC or cabal version.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type UnsetEffects = '[ NotInstalled, ParseError, NoToolVersionSet ]


runUnsetGHC :: forall appstate m a . (ReaderT appstate m (VEither UnsetEffects a) -> m (VEither UnsetEffects a))
            -> Excepts UnsetEffects (ReaderT appstate m) a
            -> m (VEither UnsetEffects a)
runUnsetGHC runLeanAppState =
    runLeanAppState
    . runE
      @UnsetEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



unset :: forall appstate m . ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         , HasDirs appstate
         , HasLog appstate
         , HasPlatformReq appstate
         )
      => UnsetCommand
      -> (ReaderT appstate m (VEither UnsetEffects ())
          -> m (VEither UnsetEffects ()))
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
unset unsetCommand runLeanAppState runLogger = case unsetCommand of
  (UnsetGHC usopts)   -> unsetOther (toUnsetOptionsNew ghc usopts)
  (UnsetCabal usopts) -> unsetOther (toUnsetOptionsNew cabal usopts)
  (UnsetHLS usopts)   -> unsetOther (toUnsetOptionsNew hls usopts)
  (UnsetStack usopts) -> unsetOther (toUnsetOptionsNew stack usopts)
  (UnsetOther usopts) -> unsetOther usopts

 where
  toUnsetOptionsNew sTool UnsetOptions{..} = UnsetOptionsNew{..}

  unsetOther UnsetOptionsNew{..} =
    runUnsetGHC runLeanAppState (liftE $ unsetTool sTool sToolTriple)
        >>= \case
              VRight _ -> do
                runLogger $ logInfo $ T.pack (prettyShow sTool) <> " successfully unset"
                pure ExitSuccess
              VLeft  e -> do
                runLogger $ logError $ T.pack $ prettyHFError e
                pure $ ExitFailure 14


