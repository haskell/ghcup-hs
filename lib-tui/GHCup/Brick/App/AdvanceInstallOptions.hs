{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.AdvanceInstallOptions where

import GHCup.Types (GHCTargetVersion)
import qualified Data.Text                     as T
import Optics.TH (makeLenses)
import URI.ByteString (URI)

data InstallOptions = InstallOptions
  { _instBindist  :: Maybe URI
  , _instSet      :: Bool
  , _instVersion :: Maybe GHCTargetVersion
  -- ^ User specified version to override default
  , _isolateDir   :: Maybe FilePath
  , _forceInstall :: Bool
  , _addConfArgs  :: [T.Text]
  , _installTargets :: T.Text
  } deriving (Eq, Show)

makeLenses ''InstallOptions
