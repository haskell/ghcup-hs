{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Types
Description : GHCup types
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Types where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Versions
import           HPath
import           URI.ByteString

import qualified Data.Text                     as T
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
  { _viTags      :: [Tag]              -- ^ version specific tag
  , _viChangeLog :: Maybe URI
  , _viSourceDL  :: Maybe DownloadInfo -- ^ source tarball
  , _viArch      :: ArchitectureSpec   -- ^ descend for binary downloads per arch
  }
  deriving (Eq, Show)


-- | A tag. These are currently attached to a version of a tool.
data Tag = Latest
         | Recommended
         | Prerelease
         | Base PVP
         | UnknownTag String  -- ^ used for upwardscompat
         deriving (Ord, Eq, Show) -- FIXME: manual JSON instance


data Architecture = A_64
                  | A_32
                  | A_PowerPC
                  | A_PowerPC64
                  | A_Sparc
                  | A_Sparc64
                  | A_ARM
                  | A_ARM64
  deriving (Eq, GHC.Generic, Ord, Show)

prettyArch :: Architecture -> String
prettyArch A_64 = "x86_64"
prettyArch A_32 = "i386"
prettyArch A_PowerPC = "powerpc"
prettyArch A_PowerPC64 = "powerpc64"
prettyArch A_Sparc = "sparc"
prettyArch A_Sparc64 = "sparc64"
prettyArch A_ARM = "arm"
prettyArch A_ARM64 = "aarch64"

data Platform = Linux LinuxDistro
              -- ^ must exit
              | Darwin
              -- ^ must exit
              | FreeBSD
  deriving (Eq, GHC.Generic, Ord, Show)

prettyPlatfrom :: Platform -> String
prettyPlatfrom (Linux distro) = "linux-" ++ prettyDistro distro
prettyPlatfrom Darwin = "darwin"
prettyPlatfrom FreeBSD = "freebsd"

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

prettyDistro :: LinuxDistro -> String
prettyDistro Debian = "debian"
prettyDistro Ubuntu = "ubuntu"
prettyDistro Mint= "mint"
prettyDistro Fedora = "fedora"
prettyDistro CentOS = "centos"
prettyDistro RedHat = "redhat"
prettyDistro Alpine = "alpine"
prettyDistro AmazonLinux = "amazon"
prettyDistro Gentoo = "gentoo"
prettyDistro Exherbo = "exherbo"
prettyDistro UnknownLinux = "unknown"


-- | An encapsulation of a download. This can be used
-- to download, extract and install a tool.
data DownloadInfo = DownloadInfo
  { _dlUri    :: URI
  , _dlSubdir :: Maybe TarDir
  , _dlHash   :: Text
  }
  deriving (Eq, Show)




    --------------
    --[ Others ]--
    --------------


-- | How to descend into a tar archive.
data TarDir = RealDir (Path Rel)
            | RegexDir String     -- ^ will be compiled to regex, the first match will "win"
            deriving (Eq, Show)


-- | Where to fetch GHCupDownloads from.
data URLSource = GHCupURL
               | OwnSource URI
               | OwnSpec GHCupInfo
               deriving Show


data Settings = Settings
  { -- set by user
    cache      :: Bool
  , noVerify   :: Bool
  , keepDirs   :: KeepDirs
  , downloader :: Downloader
  , verbose    :: Bool

    -- set on app start
  , dirs       :: Dirs
  }
  deriving Show

data Dirs = Dirs
  { baseDir  :: Path Abs
  , binDir   :: Path Abs
  , cacheDir :: Path Abs
  , logsDir  :: Path Abs
  }
  deriving Show

data KeepDirs = Always
              | Errors
              | Never
  deriving (Eq, Show, Ord)

data Downloader = Curl
                | Wget
#if defined(INTERNAL_DOWNLOADER)
                | Internal
#endif
  deriving (Eq, Show, Ord)

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

prettyPlatform :: PlatformResult -> String
prettyPlatform PlatformResult { _platform = plat, _distroVersion = Just v' }
  = show plat <> ", " <> show v'
prettyPlatform PlatformResult { _platform = plat, _distroVersion = Nothing }
  = show plat

data PlatformRequest = PlatformRequest
  { _rArch     :: Architecture
  , _rPlatform :: Platform
  , _rVersion  :: Maybe Versioning
  }
  deriving (Eq, Show)

prettyPfReq :: PlatformRequest -> String
prettyPfReq (PlatformRequest arch plat ver) =
  prettyArch arch ++ "-" ++ prettyPlatfrom plat ++ pver
 where
  pver = case ver of
           Just v' -> "-" ++ (T.unpack $ prettyV v')
           Nothing -> ""

-- | A GHC identified by the target platform triple
-- and the version.
data GHCTargetVersion = GHCTargetVersion
  { _tvTarget  :: Maybe Text
  , _tvVersion :: Version
  }
  deriving (Ord, Eq, Show)


mkTVer :: Version -> GHCTargetVersion
mkTVer = GHCTargetVersion Nothing


-- | Assembles a path of the form: <target-triple>-<version>
prettyTVer :: GHCTargetVersion -> Text
prettyTVer (GHCTargetVersion (Just t) v') = t <> "-" <> prettyVer v'
prettyTVer (GHCTargetVersion Nothing  v') = prettyVer v'

