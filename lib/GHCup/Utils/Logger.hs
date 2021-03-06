{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleContexts      #-}

{-|
Module      : GHCup.Utils.Logger
Description : logger definition
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX

Here we define our main logger.
-}
module GHCup.Utils.Logger where

import           GHCup.Types
import           GHCup.Utils

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Logger
import           HPath
import           HPath.IO
import           Prelude                 hiding ( appendFile )
import           System.Console.Pretty
import           System.IO.Error

import qualified Data.ByteString               as B


data LoggerConfig = LoggerConfig
  { lcPrintDebug :: Bool                  -- ^ whether to print debug in colorOutter
  , colorOutter  :: B.ByteString -> IO () -- ^ how to write the color output
  , rawOutter    :: B.ByteString -> IO () -- ^ how to write the full raw output
  }


myLoggerT :: LoggerConfig -> LoggingT m a -> m a
myLoggerT LoggerConfig {..} loggingt = runLoggingT loggingt mylogger
 where
  mylogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  mylogger _ _ level str' = do
    -- color output
    let l = case level of
          LevelDebug   -> toLogStr (style Bold $ color Blue "[ Debug ]")
          LevelInfo    -> toLogStr (style Bold $ color Green "[ Info  ]")
          LevelWarn    -> toLogStr (style Bold $ color Yellow "[ Warn  ]")
          LevelError   -> toLogStr (style Bold $ color Red "[ Error ]")
          LevelOther t -> toLogStr "[ " <> toLogStr t <> toLogStr " ]"
    let out = fromLogStr (l <> toLogStr " " <> str' <> toLogStr "\n")

    when (lcPrintDebug || (lcPrintDebug == False && not (level == LevelDebug)))
      $ colorOutter out

    -- raw output
    let lr = case level of
          LevelDebug   -> toLogStr "Debug: "
          LevelInfo    -> toLogStr "Info:"
          LevelWarn    -> toLogStr "Warn:"
          LevelError   -> toLogStr "Error:"
          LevelOther t -> toLogStr t <> toLogStr ":"
    let outr = fromLogStr (lr <> toLogStr " " <> str' <> toLogStr "\n")
    rawOutter outr


initGHCupFileLogging :: (MonadIO m, MonadReader AppState m) => Path Rel -> m (Path Abs)
initGHCupFileLogging context = do
  AppState {dirs = Dirs {..}} <- ask
  let logfile = logsDir </> context
  liftIO $ do
    createDirRecursive' logsDir
    hideError doesNotExistErrorType $ deleteFile logfile
    createRegularFile newFilePerms logfile
    pure logfile
