{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings   #-}

module GHCup.Logger where

import           GHCup.Types

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text               ( Text )
import           Optics

logWarn :: ( MonadReader env m
           , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
           , MonadIO m
           )
        => Text
        -> m ()

