{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.GC where


import           GHCup.Command.GC
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad (when)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
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
  , gcUnset :: Bool
  } deriving (Eq, Show)



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
  <*>
    switch
      (short 'u' <> long "unset" <> help "Remove all tool versions that are not 'set'")



    --------------
    --[ Footer ]--
    --------------


gcFooter :: String
gcFooter = [s|Discussion:
  Performs garbage collection. If no switches are specified, does nothing.|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type GCEffects = '[ NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo ]




    ------------------
    --[ Entrypoint ]--
    ------------------



gc :: ( Monad m
      , MonadMask m
      , MonadUnliftIO m
      , MonadFail m
      )
   => GCOptions
   -> (IO (AppState, IO ()), LeanAppState)
   -> m ExitCode
gc GCOptions{..} (getAppState', leanAppstate) = run (do
  when gcOldGHC (liftE rmOldGHC)
  lift $ when gcProfilingLibs rmProfilingLibs
  lift $ when gcShareDir rmShareDir
  liftE $ when gcHLSNoGHC rmHLSNoGHC
  lift $ when gcCache rmCache
  lift $ when gcTmp rmTmp
  liftE $ when gcUnset rmUnsetTools
   ) >>= \case
            (VRight _, up) -> do
              liftIO up
              pure ExitSuccess
            (VLeft e, _) -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 27
 where
  run action' = do
    (appstate', up) <- liftIO getAppState'
    r <- flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @GCEffects
                  $ action'
    pure (r, up)
  runLogger = flip runReaderT leanAppstate
