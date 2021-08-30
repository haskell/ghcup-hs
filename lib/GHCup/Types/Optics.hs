{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : GHCup.Types.Optics
Description : GHCup optics
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types.Optics where

import           GHCup.Types

import           Control.Monad.Reader
import           Data.ByteString         ( ByteString )
import           Data.Text               ( Text )
import           Optics
import           URI.ByteString
import           System.Console.Pretty

import qualified Data.Text                     as T

makePrisms ''Tool
makePrisms ''Architecture
makePrisms ''LinuxDistro
makePrisms ''Platform
makePrisms ''Tag

makeLenses ''PlatformResult
makeLenses ''DownloadInfo
makeLenses ''Tag
makeLenses ''VersionInfo

makeLenses ''GHCTargetVersion

makeLenses ''GHCupInfo

uriSchemeL' :: Lens' (URIRef Absolute) Scheme
uriSchemeL' = lensVL uriSchemeL

schemeBSL' :: Lens' Scheme ByteString
schemeBSL' = lensVL schemeBSL

authorityL' :: Lens' (URIRef a) (Maybe Authority)
authorityL' = lensVL authorityL

authorityHostL' :: Lens' Authority Host
authorityHostL' = lensVL authorityHostL

authorityPortL' :: Lens' Authority (Maybe Port)
authorityPortL' = lensVL authorityPortL

portNumberL' :: Lens' Port Int
portNumberL' = lensVL portNumberL

hostBSL' :: Lens' Host ByteString
hostBSL' = lensVL hostBSL

pathL' :: Lens' (URIRef a) ByteString
pathL' = lensVL pathL

queryL' :: Lens' (URIRef a) Query
queryL' = lensVL queryL



    ----------------------
    --[ Lens utilities ]--
    ----------------------


gets :: forall f a env m . (MonadReader env m, LabelOptic' f A_Lens env a)
     => m a
gets = asks (^. labelOptic @f)


getAppState :: MonadReader AppState m => m AppState
getAppState = ask


getLeanAppState :: ( MonadReader env m
                   , LabelOptic' "settings"    A_Lens env Settings
                   , LabelOptic' "dirs"        A_Lens env Dirs
                   , LabelOptic' "keyBindings" A_Lens env KeyBindings
                   , LabelOptic' "loggerConfig" A_Lens env LoggerConfig
                   )
                => m LeanAppState
getLeanAppState = do
  s <- gets @"settings"
  d <- gets @"dirs"
  k <- gets @"keyBindings"
  l <- gets @"loggerConfig"
  pure (LeanAppState s d k l)


getSettings :: ( MonadReader env m
               , LabelOptic' "settings" A_Lens env Settings
               )
            => m Settings
getSettings = gets @"settings"


getDirs :: ( MonadReader env m
           , LabelOptic' "dirs" A_Lens env Dirs
           )
        => m Dirs
getDirs = gets @"dirs"


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
  let style' = case logLevel of
        Debug   -> style Bold . color Blue
        Info    -> style Bold . color Green
        Warn    -> style Bold . color Yellow
        Error   -> style Bold . color Red
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
    $ liftIO $ colorOutter out

  -- raw output
  let lr = case logLevel of
        Debug   -> "Debug:"
        Info    -> "Info:"
        Warn    -> "Warn:"
        Error   -> "Error:"
  let outr = lr <> " " <> msg <> "\n"
  liftIO $ rawOutter outr



getLogCleanup :: ( MonadReader env m
                 , LabelOptic' "logCleanup" A_Lens env (IO ())
                 )
              => m (IO ())
getLogCleanup = gets @"logCleanup"


getKeyBindings :: ( MonadReader env m
                  , LabelOptic' "keyBindings" A_Lens env KeyBindings
                  )
               => m KeyBindings
getKeyBindings = gets @"keyBindings"


getGHCupInfo :: ( MonadReader env m
                , LabelOptic' "ghcupInfo" A_Lens env GHCupInfo
                )
             => m GHCupInfo
getGHCupInfo = gets @"ghcupInfo"


getPlatformReq :: ( MonadReader env m
                  , LabelOptic' "pfreq" A_Lens env PlatformRequest
                  )
               => m PlatformRequest
getPlatformReq = gets @"pfreq"


type HasSettings env = (LabelOptic' "settings" A_Lens env Settings)
type HasDirs env = (LabelOptic' "dirs" A_Lens env Dirs)
type HasKeyBindings env = (LabelOptic' "keyBindings" A_Lens env KeyBindings)
type HasGHCupInfo env = (LabelOptic' "ghcupInfo" A_Lens env GHCupInfo)
type HasPlatformReq env = (LabelOptic' "pfreq" A_Lens env PlatformRequest)
type HasLog env = (LabelOptic' "loggerConfig" A_Lens env LoggerConfig)


getCache :: (MonadReader env m, HasSettings env) => m Bool
getCache = getSettings <&> cache


getDownloader :: (MonadReader env m, HasSettings env) => m Downloader
getDownloader = getSettings <&> downloader


instance LabelOptic "dirs" A_Lens Dirs Dirs Dirs Dirs where
  labelOptic = lens id (\_ d -> d)
