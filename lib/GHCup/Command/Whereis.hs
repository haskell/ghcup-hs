{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Command.Whereis
Description : GHCup whereis
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Whereis where


import GHCup.Errors
import GHCup.Legacy.Utils
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions        hiding ( patch )
import Prelude              hiding ( abs, writeFile )
import System.Environment
import System.FilePath

import qualified Data.Text as T





    ---------------
    --[ Whereis ]--
    ---------------



-- | Reports the binary location of a given tool:
--
--   * for GHC, this reports: @~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * for cabal, this reports @~\/.ghcup\/bin\/cabal-\<ver\>@
--   * for hls, this reports @~\/.ghcup\/bin\/haskell-language-server-wrapper-\<ver\>@
--   * for stack, this reports @~\/.ghcup\/bin\/stack-\<ver\>@
--   * for ghcup, this reports the location of the currently running executable
whereIsTool :: ( MonadReader env m
               , HasDirs env
               , HasLog env
               , HasPlatformReq env
               , MonadIOish m
               )
            => Tool
            -> GHCTargetVersion
            -> Excepts '[NotInstalled, ParseError, NoInstallInfo] m FilePath
whereIsTool tool ver@GHCTargetVersion {..} = do
  dirs <- lift getDirs
  sSpec <- lift $ getSymlinkSpecPortable tool ver
  toolDir <- fromGHCupPath <$> lift (toolInstallDestination tool ver)

  case sSpec of
    (SymlinkSpec{..}:_) ->
      pure $ toolDir </> _slTarget
    _ ->
      case tool of
        Tool "ghc" -> do
          whenM (lift $ fmap not $ ghcInstalled ver)
            $ throwE (NotInstalled ghc ver)
          bdir <- fromGHCupPath <$> lift (ghcupGHCDir ver)
          pure (bdir </> "bin" </> ghcBinaryName ver)
        Tool "cabal" -> do
          whenM (lift $ fmap not $ cabalInstalled _tvVersion)
            $ throwE (NotInstalled cabal (GHCTargetVersion Nothing _tvVersion))
          pure (binDir dirs </> "cabal-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
        Tool "hls" -> do
          whenM (lift $ fmap not $ hlsInstalled _tvVersion)
            $ throwE (NotInstalled hls (GHCTargetVersion Nothing _tvVersion))
          ifM (lift $ isLegacyHLS _tvVersion)
            (pure (binDir dirs </> "haskell-language-server-wrapper-" <> T.unpack (prettyVer _tvVersion) <> exeExt))
            $ do
              bdir <- fromGHCupPath <$> lift (ghcupHLSDir _tvVersion)
              pure (bdir </> "bin" </> "haskell-language-server-wrapper" <> exeExt)

        Tool "stack" -> do
          whenM (lift $ fmap not $ stackInstalled _tvVersion)
            $ throwE (NotInstalled stack (GHCTargetVersion Nothing _tvVersion))
          pure (binDir dirs </> "stack-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
        Tool "ghcup" -> do
          currentRunningExecPath <- liftIO getExecutablePath
          liftIO $ canonicalizePath currentRunningExecPath
        -- TODO
        _ -> pure ""

