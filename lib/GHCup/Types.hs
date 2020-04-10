{-# LANGUAGE DeriveGeneric #-}

module GHCup.Types where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Versions
import           HPath
import           URI.ByteString

import qualified GHC.Generics                  as GHC



    --------------------
    --[ GHCInfo Tree ]--
    --------------------


data GHCupInfo = GHCupInfo
  { _toolRequirements :: ToolRequirements
  , _ghcupDownloads   :: GHCupDownloads
  }
  deriving (Show, GHC.Generic)



    -------------------------
    --[ Requirements Tree ]--
    -------------------------


type ToolRequirements = Map Tool ToolReqVersionSpec
type ToolReqVersionSpec = Map (Maybe Version) PlatformReqSpec
type PlatformReqSpec = Map Platform PlatformReqVersionSpec
type PlatformReqVersionSpec = Map (Maybe Versioning) Requirements


data Requirements = Requirements
  { _distroPKGs :: [Text]
  , _notes      :: Text
  }
  deriving (Show, GHC.Generic)





    ---------------------
    --[ Download Tree ]--
    ---------------------


-- | Description of all binary and source downloads. This is a tree
-- of nested maps.
type GHCupDownloads = Map Tool ToolVersionSpec
type ToolVersionSpec = Map Version VersionInfo
type ArchitectureSpec = Map Architecture PlatformSpec
type PlatformSpec = Map Platform PlatformVersionSpec
type PlatformVersionSpec = Map (Maybe Versioning) DownloadInfo


-- | An installable tool.
data Tool = GHC
          | Cabal
          | GHCup
  deriving (Eq, GHC.Generic, Ord, Show)


-- | All necessary information of a tool version, including
-- source download and per-architecture downloads.
data VersionInfo = VersionInfo
  { _viTags     :: [Tag]              -- ^ version specific tag
  , _viSourceDL :: Maybe DownloadInfo -- ^ source tarball
  , _viArch     :: ArchitectureSpec   -- ^ descend for binary downloads per arch
  }
  deriving (Eq, Show)


-- | A tag. These are currently attached to a version of a tool.
data Tag = Latest
         | Recommended
         deriving (Ord, Eq, Show)


data Architecture = A_64
                  | A_32
  deriving (Eq, GHC.Generic, Ord, Show)


data Platform = Linux LinuxDistro
              -- ^ must exit
              | Darwin
              -- ^ must exit
              | FreeBSD
  deriving (Eq, GHC.Generic, Ord, Show)

data LinuxDistro = Debian
                 | Ubuntu
                 | Mint
                 | Fedora
                 | CentOS
                 | RedHat
                 | Alpine
                 | AmazonLinux
                 -- rolling
                 | Gentoo
                 | Exherbo
                 -- not known
                 | UnknownLinux
                 -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show)


-- | An encapsulation of a download. This can be used
-- to download, extract and install a tool.
data DownloadInfo = DownloadInfo
  { _dlUri    :: URI
  , _dlSubdir :: Maybe (Path Rel)
  , _dlHash   :: Text
  }
  deriving (Eq, Show)




    --------------
    --[ Others ]--
    --------------


-- | Where to fetch GHCupDownloads from.
data URLSource = GHCupURL
               | OwnSource URI
               | OwnSpec GHCupInfo
               deriving Show


data Settings = Settings
  { cache    :: Bool
  , noVerify :: Bool
  }
  deriving Show


data DebugInfo = DebugInfo
  { diBaseDir  :: Path Abs
  , diBinDir   :: Path Abs
  , diGHCDir   :: Path Abs
  , diCacheDir :: Path Abs
  , diArch     :: Architecture
  , diPlatform :: PlatformResult
  }
  deriving Show


data SetGHC = SetGHCOnly  -- ^ unversioned 'ghc'
            | SetGHC_XY   -- ^ ghc-x.y
            | SetGHC_XYZ  -- ^ ghc-x.y.z
            deriving (Eq, Show)


data PlatformResult = PlatformResult
  { _platform      :: Platform
  , _distroVersion :: Maybe Versioning
  }
  deriving (Eq, Show)

data PlatformRequest = PlatformRequest
  { _rArch     :: Architecture
  , _rPlatform :: Platform
  , _rVersion  :: Maybe Versioning
  }
  deriving (Eq, Show)
