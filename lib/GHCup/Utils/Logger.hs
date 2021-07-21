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
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Char               ( ord )
import           Prelude                 hiding ( appendFile )
import           System.Console.Pretty
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix

import qualified Data.ByteString               as B
import GHCup.Utils.Prelude


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
    let style' = case level of
          LevelDebug   -> style Bold . color Blue
          LevelInfo    -> style Bold . color Green
          LevelWarn    -> style Bold . color Yellow
          LevelError   -> style Bold . color Red
          LevelOther _ -> id
    let l = case level of
          LevelDebug   -> toLogStr (style' "[ Debug ]")
          LevelInfo    -> toLogStr (style' "[ Info  ]")
          LevelWarn    -> toLogStr (style' "[ Warn  ]")
          LevelError   -> toLogStr (style' "[ Error ]")
          LevelOther t -> toLogStr "[ " <> toLogStr t <> toLogStr " ]"
    let strs = fmap toLogStr . B.split (fromIntegral $ ord '\n') . fromLogStr $ str'
    let out = case strs of
                [] -> B.empty
                (x:xs) -> fromLogStr
                  . foldr (\a b -> a <> toLogStr "\n" <> b) mempty
                  . ((l <> toLogStr " " <> x) :)
                  . fmap (\line' -> toLogStr (style' "[ ...   ] ") <> line' )
                  $ xs

    when (lcPrintDebug || (not lcPrintDebug && (level /= LevelDebug)))
      $ colorOutter out

    -- raw output
    let lr = case level of
          LevelDebug   -> toLogStr "Debug:"
          LevelInfo    -> toLogStr "Info:"
          LevelWarn    -> toLogStr "Warn:"
          LevelError   -> toLogStr "Error:"
          LevelOther t -> toLogStr t <> toLogStr ":"
    let outr = fromLogStr (lr <> toLogStr " " <> str' <> toLogStr "\n")
    rawOutter outr


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
  forM_ logFiles $ hideError doesNotExistErrorType . rmFile . (logsDir </>)

  liftIO $ writeFile logfile ""
  pure logfile
