{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

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
module GHCup.Utils.Logger where

import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils.File
import           GHCup.Utils.String.QQ

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Prelude                 hiding ( appendFile )
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix

import qualified Data.ByteString               as B
import GHCup.Utils.Prelude



initGHCupFileLogging :: ( MonadReader env m
                        , HasDirs env
                        , MonadIO m
                        , MonadMask m
                        ) => m FilePath
initGHCupFileLogging = do
  Dirs { logsDir } <- getDirs
  let logfile = logsDir </> "ghcup.log"
  logFiles <- liftIO $ findFiles
    logsDir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^.*\.log$|] :: B.ByteString)
    )
  forM_ logFiles $ hideError doesNotExistErrorType . recycleFile . (logsDir </>)

  liftIO $ writeFile logfile ""
  pure logfile
