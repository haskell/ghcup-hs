{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : GHCup.Utils.Logger
Description : logger definition
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

Here we define our main logger.
-}
module GHCup.Prelude.Logger
  ( module GHCup.Prelude.Logger
  , module GHCup.Prelude.Logger.Internal
  )
where

import           GHCup.Prelude.Logger.Internal
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils.Dirs (fromGHCupPath)
import           GHCup.Prelude.Internal
import           GHCup.Prelude.File.Search (findFiles)
import           GHCup.Prelude.File (recycleFile)
import           GHCup.Prelude.String.QQ

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Prelude                 hiding ( appendFile )
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix

import qualified Data.ByteString               as B



initGHCupFileLogging :: ( MonadReader env m
                        , HasDirs env
                        , MonadIO m
                        , MonadMask m
                        ) => m FilePath
initGHCupFileLogging = do
  Dirs { logsDir } <- getDirs
  let logfile = fromGHCupPath logsDir </> "ghcup.log"
  logFiles <- liftIO $ findFiles
    (fromGHCupPath logsDir)
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^.*\.log$|] :: B.ByteString)
    )
  forM_ logFiles $ hideError doesNotExistErrorType . recycleFile . (fromGHCupPath logsDir </>)

  liftIO $ writeFile logfile ""
  pure logfile
