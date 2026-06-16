{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Fixup where


import GHCup.Command.Fixup
import GHCup.Errors
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
import           Options.Applicative     hiding ( style, ParseError )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import Options.Applicative.Pretty.Shim ( text )
import Control.Exception.Safe (MonadMask)

import qualified Data.Text                     as T





    ----------------
    --[ Commands ]--
    ----------------


data FixupCommand
  = FixupSymlinks
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


fixupP :: Parser FixupCommand
fixupP =
  subparser
      (  command
          "symlinks"
          (   (const FixupSymlinks)
          <$> info
                helper
                (  progDesc "Fix up symlinks in bin/"
                <> footerDoc (Just $ text toolFooter)
                )
          )
      )
 where
  toolFooter :: String
  toolFooter = [s|Discussion:
  Fix up common issues.

Examples:
  ghcup fixup symlinks|]







    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type FixupEffects = '[MalformedInstallInfo, ParseError]




    ------------------
    --[ Entrypoint ]--
    ------------------



fixup :: ( Monad m
         , MonadMask m
         , MonadUnliftIO m
         , MonadFail m
         )
      => FixupCommand
      -> (IO (AppState, IO ()), LeanAppState)
      -> m ExitCode
fixup command' (getAppState', leanAppstate) = run (do
     case command' of
       FixupSymlinks ->
         liftE fixupSymlinks
   ) >>= \case
            (VRight _, up) -> do
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
                    @FixupEffects
                  $ action'
    pure (r, up)

