{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : GHCup.Command.DebugInfo
Description : GHCup installation functions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains the main functions that correspond
to the command line interface, like installation, listing versions
and so on.

These are the entry points.
-}
module GHCup.Command.DebugInfo where


import GHCup.Errors
import GHCup.Hardcoded.URLs
import GHCup.Prelude
import GHCup.Query.System
import GHCup.Types
import GHCup.Types.JSON
    ()
import GHCup.Types.Optics

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Variant.Excepts
import Prelude                      hiding ( abs, writeFile )




    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: ( Alternative m
                , MonadFail m
                , MonadReader env m
                , HasDirs env
                , HasLog env
                , MonadCatch m
                , MonadIO m
                )
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  diDirs <- lift getDirs
  let diChannels = fmap (\c -> (c, channelURL c)) [minBound..maxBound]
  let diShimGenURL = shimGenURL
  diArch         <- lE getArchitecture
  diPlatform     <- liftE getPlatform
  pure $ DebugInfo { .. }
