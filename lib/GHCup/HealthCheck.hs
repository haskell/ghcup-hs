{-# LANGUAGE CPP                   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : GHCup.HealthCheck
Description : HealthCheck for GHCup
License     : LGPL-3.0
Stability   : experimental
Portability : portable
-}
module GHCup.HealthCheck where

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude.Logger
import           GHCup.Version

import           Conduit (sourceToList)
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Versions                hiding ( patch )
import           GHC.IO.Exception
import           Data.Variant.Excepts
import           Optics
import Text.PrettyPrint.Annotated.HughesPJClass (Pretty, pPrint, text)


data HealthCheckResult = HealthCheckResult {
    canFetchMetadata :: VEither '[DownloadFailed] ()
  } deriving (Show)

instance Pretty HealthCheckResult where
  pPrint (HealthCheckResult {..}) = text ""

runHealthCheck :: ( MonadReader env m
                  , HasDirs env
                  , HasLog env
                  , MonadIO m
                  , MonadMask m
                  , MonadFail m
                  , MonadUnliftIO m
                  )
               => Bool
               -> Excepts
                      '[ DigestError
                       , ContentLengthError
                       , GPGError
                       , DownloadFailed
                       , NoDownload
                       ]
                       m HealthCheckResult
runHealthCheck offline = do
  -- TODO: implement
  let canFetchMetadata = VRight ()

  pure $ HealthCheckResult {..}

