{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Rm where




import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.OptParse.Common

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Data.Versions           hiding ( str )
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           Optics

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)




    ----------------
    --[ Commands ]--
    ----------------


data RmCommand = RmGHC RmOptions
               | RmCabal Version
               | RmHLS Version
               | RmStack Version




    ---------------
    --[ Options ]--
    ---------------


data RmOptions = RmOptions
  { ghcVer :: GHCTargetVersion
  }




    ---------------
    --[ Parsers ]--
    ---------------


rmParser :: Parser (Either RmCommand RmOptions)
rmParser =
  (Left <$> subparser
      (  command
          "ghc"
          (RmGHC <$> info (rmOpts (Just GHC) <**> helper) (progDesc "Remove GHC version"))
      <> command
           "cabal"
           (   RmCabal
           <$> info (versionParser' (Just ListInstalled) (Just Cabal) <**> helper)
                    (progDesc "Remove Cabal version")
           )
      <> command
           "hls"
           (   RmHLS
           <$> info (versionParser' (Just ListInstalled) (Just HLS) <**> helper)
                    (progDesc "Remove haskell-language-server version")
           )
      <> command
           "stack"
           (   RmStack
           <$> info (versionParser' (Just ListInstalled) (Just Stack) <**> helper)
                    (progDesc "Remove stack version")
           )
      )
    )
    <|> (Right <$> rmOpts Nothing)



rmOpts :: Maybe Tool -> Parser RmOptions
rmOpts tool = RmOptions <$> ghcVersionArgument (Just ListInstalled) tool




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


type RmEffects = '[ NotInstalled, UninstallFailed ]


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
   => Either RmCommand RmOptions
   -> (ReaderT AppState m (VEither RmEffects (Maybe VersionInfo))
       -> m (VEither RmEffects (Maybe VersionInfo)))
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
rm rmCommand runAppState runLogger = case rmCommand of
  (Right rmopts) -> do
    runLogger (logWarn "This is an old-style command for removing GHC. Use 'ghcup rm ghc' instead.")
    rmGHC' rmopts
  (Left (RmGHC rmopts)) -> rmGHC' rmopts
  (Left (RmCabal rmopts)) -> rmCabal' rmopts
  (Left (RmHLS rmopts)) -> rmHLS' rmopts
  (Left (RmStack rmopts)) -> rmStack' rmopts

 where
  rmGHC' RmOptions{..} =
    runRm runAppState (do
        liftE $
          rmGHCVer ghcVer
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        pure (getVersionInfo (_tvVersion ghcVer) GHC dls)
      )
      >>= \case
            VRight vi -> do
              runLogger $ logGHCPostRm ghcVer
              postRmLog vi
              pure ExitSuccess
            VLeft  e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 7

  rmCabal' tv =
    runRm runAppState (do
        liftE $
          rmCabalVer tv
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        pure (getVersionInfo tv Cabal dls)
      )
      >>= \case
            VRight vi -> do
              postRmLog vi
              pure ExitSuccess
            VLeft  e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 15

  rmHLS' tv =
    runRm runAppState (do
        liftE $
          rmHLSVer tv
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        pure (getVersionInfo tv HLS dls)
      )
      >>= \case
            VRight vi -> do
              postRmLog vi
              pure ExitSuccess
            VLeft  e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 15

  rmStack' tv =
    runRm runAppState (do
        liftE $
          rmStackVer tv
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        pure (getVersionInfo tv Stack dls)
      )
      >>= \case
            VRight vi -> do
              postRmLog vi
              pure ExitSuccess
            VLeft  e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 15

  postRmLog vi =
    forM_ (view viPostRemove =<< vi) $ \msg ->
      runLogger $ logInfo msg
