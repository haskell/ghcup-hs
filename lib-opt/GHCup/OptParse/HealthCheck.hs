{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.HealthCheck where


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
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)
import Text.PrettyPrint.Annotated.HughesPJClass (prettyShow)





    ---------------
    --[ Options ]--
    ---------------


data HealtCheckOptions = HealtCheckOptions
  { hcOffline :: Bool
  } deriving (Eq, Show)



    ---------------
    --[ Parsers ]--
    ---------------


hcP :: Parser HealtCheckOptions
hcP =
  HealtCheckOptions
  <$>
    switch
      (short 'o' <> long "offline" <> help "Only do checks that don't require internet")



    --------------
    --[ Footer ]--
    --------------


hcFooter :: String
hcFooter = [s|Discussion:
  Performs various health checks. Good for attaching to bug reports.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type HCEffects = '[ DigestError
                  , ContentLengthError
                  , GPGError
                  , DownloadFailed
                  , NoDownload
                  ]



runHC :: MonadUnliftIO m
      => (ReaderT LeanAppState m (VEither HCEffects a) -> m (VEither HCEffects a))
      -> Excepts HCEffects (ResourceT (ReaderT LeanAppState m)) a
      -> m (VEither HCEffects a)
runHC runLeanAppState =
  runLeanAppState
    . runResourceT
    . runE
      @HCEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



hc :: ( Monad m
      , MonadMask m
      , MonadUnliftIO m
      , MonadFail m
      )
   => HealtCheckOptions
   -> (forall a. ReaderT LeanAppState m (VEither HCEffects a) -> m (VEither HCEffects a))
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
hc HealtCheckOptions{..} runAppState runLogger = runHC runAppState (do
     runHealthCheck hcOffline
   ) >>= \case
            VRight r -> do
                  liftIO $ print $ prettyShow r
                  pure ExitSuccess
            VLeft e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 27

