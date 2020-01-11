{-# LANGUAGE TemplateHaskell #-}

module GHCup.Types.Optics where

import           GHCup.Types

import           Data.ByteString                ( ByteString )
import           Optics
import           URI.ByteString

makePrisms ''Tool
makePrisms ''Architecture
makePrisms ''LinuxDistro
makePrisms ''Platform
makePrisms ''Tag

makeLenses ''PlatformResult
makeLenses ''DownloadInfo
makeLenses ''Tag
makeLenses ''VersionInfo


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
