{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.GC where


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
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask)





    ---------------
    --[ Options ]--
    ---------------


data GCOptions = GCOptions
  { gcOldGHC :: Bool
  , gcProfilingLibs :: Bool
  , gcShareDir :: Bool
  , gcHLSNoGHC :: Bool
  , gcCache :: Bool
  , gcTmp :: Bool
  }



    ---------------
    --[ Parsers ]--
    ---------------


gcP :: Parser GCOptions
gcP =
  GCOptions
  <$>
    switch
      (short 'o' <> long "ghc-old" <> help "Remove GHC versions marked as 'old'")
  <*>
    switch
      (short 'p' <> long "profiling-libs" <> help "Remove profiling libs of GHC versions")
  <*>
    switch
      (short 's' <> long "share-dir" <> help "Remove GHC share directories (documentation)")
  <*>
    switch
      (short 'h' <> long "hls-no-ghc" <> help "Remove HLS versions that don't have a corresponding installed GHC version")
  <*>
    switch
      (short 'c' <> long "cache" <> help "GC the GHCup cache")
  <*>
    switch
      (short 't' <> long "tmpdirs" <> help "Remove tmpdir leftovers")



    --------------
    --[ Footer ]--
    --------------


gcFooter :: String
gcFooter = [s|Discussion:
  Performs garbage collection. If no switches are specified, does nothing.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type GCEffects = '[ NotInstalled, UninstallFailed ]


runGC :: MonadUnliftIO m
      => (ReaderT AppState m (VEither GCEffects a) -> m (VEither GCEffects a))
      -> Excepts GCEffects (ResourceT (ReaderT AppState m)) a
      -> m (VEither GCEffects a)
runGC runAppState =
  runAppState
    . runResourceT
    . runE
      @GCEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



gc :: ( Monad m
      , MonadMask m
      , MonadUnliftIO m
      , MonadFail m
      )
   => GCOptions
   -> (forall a. ReaderT AppState m (VEither GCEffects a) -> m (VEither GCEffects a))
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
gc GCOptions{..} runAppState runLogger = runGC runAppState (do
  when gcOldGHC (liftE rmOldGHC)
  lift $ when gcProfilingLibs rmProfilingLibs
  lift $ when gcShareDir rmShareDir
  liftE $ when gcHLSNoGHC rmHLSNoGHC
  lift $ when gcCache rmCache
  lift $ when gcTmp rmTmp
   ) >>= \case
            VRight _ -> do
                  pure ExitSuccess
            VLeft e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 27
