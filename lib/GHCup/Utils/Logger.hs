{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
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
module GHCup.Utils.Logger where

import           GHCup.Types
import           GHCup.Types.Optics
import {-# SOURCE #-} GHCup.System.Directory
import           GHCup.Utils.String.QQ

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text               ( Text )
import           Optics
import           Prelude                 hiding ( appendFile )
import           System.Console.Pretty
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix

import qualified Data.ByteString               as B
import GHCup.Utils.Prelude
import qualified Data.Text                     as T

logInfo :: ( MonadReader env m
           , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
           , MonadIO m
           )
        => Text
        -> m ()
logInfo = logInternal Info

logWarn :: ( MonadReader env m
           , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
           , MonadIO m
           )
        => Text
        -> m ()
logWarn = logInternal Warn

logDebug :: ( MonadReader env m
            , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
            , MonadIO m
            )
         => Text
         -> m ()
logDebug = logInternal Debug

logError :: ( MonadReader env m
            , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
            , MonadIO m
            )
         => Text
         -> m ()
logError = logInternal Error


logInternal :: ( MonadReader env m
               , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
               , MonadIO m
               ) => LogLevel
                 -> Text
                 -> m ()
logInternal logLevel msg = do
  LoggerConfig {..} <- gets @"loggerConfig"
  let color' c = if fancyColors then color c else id
  let style' = case logLevel of
        Debug   -> style Bold . color' Blue
        Info    -> style Bold . color' Green
        Warn    -> style Bold . color' Yellow
        Error   -> style Bold . color' Red
  let l = case logLevel of
        Debug   -> style' "[ Debug ]"
        Info    -> style' "[ Info  ]"
        Warn    -> style' "[ Warn  ]"
        Error   -> style' "[ Error ]"
  let strs = T.split (== '\n') msg
  let out = case strs of
              [] -> T.empty
              (x:xs) -> 
                  foldr (\a b -> a <> "\n" <> b) mempty
                . ((l <> " " <> x) :)
                . fmap (\line' -> style' "[ ...   ] " <> line' )
                $ xs

  when (lcPrintDebug || (not lcPrintDebug && (logLevel /= Debug)))
    $ liftIO $ consoleOutter out

  -- raw output
  let lr = case logLevel of
        Debug   -> "Debug:"
        Info    -> "Info:"
        Warn    -> "Warn:"
        Error   -> "Error:"
  let outr = lr <> " " <> msg <> "\n"
  liftIO $ fileOutter outr


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
