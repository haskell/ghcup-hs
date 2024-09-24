{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : GHCup.Types
Description : GHCup types
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Types
  ( module GHCup.Types
#if defined(BRICK)
  , Key(..)
  , Modifier(..)
#endif
  , ArchiveResult(..)
  )
  where

import           GHCup.Types.Stack              ( SetupInfo )
import           GHCup.Utils.Tar.Types          ( ArchiveResult(..) )
import {-# SOURCE #-} GHCup.Utils.Dirs          ( fromGHCupPath, GHCupPath )

import           Control.DeepSeq                ( NFData, rnf )
import           Data.Map.Strict                ( Map )
import           Data.List.NonEmpty             ( NonEmpty (..) )
import           Data.Time.Calendar             ( Day )
import           Data.Text                      ( Text )
import           Data.Versions
import           GHC.IO.Exception               ( ExitCode )
import           Optics                         ( makeLenses )
import           Text.PrettyPrint.HughesPJClass (Pretty, pPrint, text)
import           URI.ByteString
#if defined(BRICK)
import           Graphics.Vty                   ( Key(..), Modifier(..) )
#endif

import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified GHC.Generics                  as GHC
import qualified Data.List.NonEmpty            as NE

#if !defined(BRICK)
data Key = KEsc  | KChar Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq,Show,Read,Ord,GHC.Generic)

data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Read,Ord,GHC.Generic)
#endif

data KeyCombination = KeyCombination { key :: Key, mods :: [Modifier] }
    deriving (Eq,Show,Read,Ord,GHC.Generic)



    --------------------
    --[ GHCInfo Tree ]--
    --------------------


data GHCupInfo = GHCupInfo
  { _toolRequirements :: ToolRequirements
  , _ghcupDownloads   :: GHCupDownloads
  , _metadataUpdate   :: Maybe URI
  }
  deriving (Show, GHC.Generic, Eq)

instance NFData GHCupInfo



    -------------------------
    --[ Requirements Tree ]--
    -------------------------


type ToolRequirements = Map Tool ToolReqVersionSpec
type ToolReqVersionSpec = Map (Maybe Version) PlatformReqSpec
type PlatformReqSpec = Map Platform PlatformReqVersionSpec
type PlatformReqVersionSpec = Map (Maybe VersionRange) Requirements


data Requirements = Requirements
  { _distroPKGs :: [Text]
  , _notes      :: Text
  }
  deriving (Show, GHC.Generic, Eq)

instance NFData Requirements





    ---------------------
    --[ Download Tree ]--
    ---------------------


-- | Description of all binary and source downloads. This is a tree
-- of nested maps.
type GHCupDownloads = Map Tool ToolVersionSpec
type ToolVersionSpec = Map GHCTargetVersion VersionInfo
type ArchitectureSpec = Map Architecture PlatformSpec
type PlatformSpec = Map Platform PlatformVersionSpec
type PlatformVersionSpec = Map (Maybe VersionRange) DownloadInfo


-- | An installable tool.
data Tool = GHC
          | Cabal
          | GHCup
          | HLS
          | Stack
  deriving (Eq, GHC.Generic, Ord, Show, Enum, Bounded)

instance Pretty Tool where
  pPrint GHC = text "ghc"
  pPrint Cabal = text "cabal"
  pPrint GHCup = text "ghcup"
  pPrint HLS = text "hls"
  pPrint Stack = text "stack"

instance NFData Tool


-- | All necessary information of a tool version, including
-- source download and per-architecture downloads.
data VersionInfo = VersionInfo
  { _viTags        :: [Tag]              -- ^ version specific tag
  , _viReleaseDay  :: Maybe Day
  , _viChangeLog   :: Maybe URI
  , _viSourceDL    :: Maybe DownloadInfo -- ^ source tarball
  , _viTestDL      :: Maybe DownloadInfo -- ^ test tarball
  , _viArch        :: ArchitectureSpec   -- ^ descend for binary downloads per arch
  -- informative messages
  , _viPreInstall  :: Maybe Text
  , _viPostInstall :: Maybe Text
  , _viPostRemove  :: Maybe Text
  , _viPreCompile  :: Maybe Text
  }
  deriving (Eq, GHC.Generic, Show)

instance NFData VersionInfo


-- | A tag. These are currently attached to a version of a tool.
data Tag = Latest             -- ^ the latest version of a tool (unique per tool)
         | Recommended        -- ^ the recommended version of a tool (unique per tool)
         | Prerelease         -- ^ denotes a prerelease version
                              --   (a version should either be 'Prerelease' or
                              --   'LatestPrerelease', but not both)
         | LatestPrerelease   -- ^ the latest prerelease (unique per tool)
         | Nightly            -- ^ denotes a nightly version
                              --   (a version should either be 'Nightly' or
                              --   'LatestNightly', but not both)
         | LatestNightly      -- ^ the latest nightly (unique per tool)
         | Base PVP           -- ^ the base version shipped with GHC
         | Old                -- ^ old versions are hidden by default in TUI
         | UnknownTag String  -- ^ used for upwardscompat
         deriving (Ord, Eq, GHC.Generic, Show) -- FIXME: manual JSON instance

instance NFData Tag

tagToString :: Tag -> String
tagToString Recommended        = "recommended"
tagToString Latest             = "latest"
tagToString Prerelease         = "prerelease"
tagToString Nightly            = "nightly"
tagToString (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
tagToString (UnknownTag t    ) = t
tagToString LatestPrerelease   = "latest-prerelease"
tagToString LatestNightly      = "latest-nightly"
tagToString Old                = ""

instance Pretty Tag where
  pPrint Recommended        = text "recommended"
  pPrint Latest             = text "latest"
  pPrint Prerelease         = text "prerelease"
  pPrint Nightly            = text "nightly"
  pPrint (Base       pvp'') = text ("base-" ++ T.unpack (prettyPVP pvp''))
  pPrint (UnknownTag t    ) = text t
  pPrint LatestPrerelease   = text "latest-prerelease"
  pPrint LatestNightly      = text "latest-prerelease"
  pPrint Old                = mempty

data Architecture = A_64
                  | A_32
                  | A_PowerPC
                  | A_PowerPC64
                  | A_Sparc
                  | A_Sparc64
                  | A_ARM
                  | A_ARM64
  deriving (Eq, GHC.Generic, Ord, Show, Bounded, Enum)

instance NFData Architecture

archToString :: Architecture -> String
archToString A_64 = "x86_64"
archToString A_32 = "i386"
archToString A_PowerPC = "powerpc"
archToString A_PowerPC64 = "powerpc64"
archToString A_Sparc = "sparc"
archToString A_Sparc64 = "sparc64"
archToString A_ARM = "arm"
archToString A_ARM64 = "aarch64"

instance Pretty Architecture where
  pPrint = text . archToString

data Platform = Linux LinuxDistro
              -- ^ must exit
              | Darwin
              -- ^ must exit
              | FreeBSD
              | Windows
              -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData Platform

platformToString :: Platform -> String
platformToString (Linux distro) = "linux-" ++ distroToString distro
platformToString Darwin = "darwin"
platformToString FreeBSD = "freebsd"
platformToString Windows = "windows"

instance Pretty Platform where
  pPrint = text . platformToString

data LinuxDistro = Debian
                 | Ubuntu
                 | Mint
                 | Fedora
                 | CentOS
                 | RedHat
                 | Alpine
                 | AmazonLinux
                 | Rocky
                 | Void
                 -- rolling
                 | Gentoo
                 | Exherbo
                 -- not known
                 | UnknownLinux
                 -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show, Enum, Bounded)

allDistros :: [LinuxDistro]
allDistros = enumFromTo minBound maxBound

instance NFData LinuxDistro

distroToString :: LinuxDistro -> String
distroToString Debian = "debian"
distroToString Ubuntu = "ubuntu"
distroToString Mint = "mint"
distroToString Fedora = "fedora"
distroToString CentOS = "centos"
distroToString RedHat = "redhat"
distroToString Alpine = "alpine"
distroToString AmazonLinux = "amazon"
distroToString Rocky = "rocky"
distroToString Void = "void"
distroToString Gentoo = "gentoo"
distroToString Exherbo = "exherbo"
distroToString UnknownLinux = "unknown"

instance Pretty LinuxDistro where
  pPrint = text . distroToString


-- | An encapsulation of a download. This can be used
-- to download, extract and install a tool.
data DownloadInfo = DownloadInfo
  { _dlUri    :: URI
  , _dlSubdir :: Maybe TarDir
  , _dlHash   :: Text
  , _dlCSize  :: Maybe Integer
  , _dlOutput :: Maybe FilePath
  }
  deriving (Eq, Ord, GHC.Generic, Show)

instance NFData DownloadInfo



    --------------
    --[ Others ]--
    --------------

data DownloadMirror = DownloadMirror {
     authority :: Authority
   , pathPrefix :: Maybe Text
} deriving (Eq, Ord, GHC.Generic, Show)

instance NFData DownloadMirror

newtype DownloadMirrors = DM (Map Text DownloadMirror)
  deriving (Eq, Ord, GHC.Generic, Show)

instance NFData DownloadMirrors

instance NFData UserInfo
instance NFData Host
instance NFData Port
instance NFData Authority


-- | How to descend into a tar archive.
data TarDir = RealDir FilePath
            | RegexDir String     -- ^ will be compiled to regex, the first match will "win"
            deriving (Eq, Ord, GHC.Generic, Show)

instance NFData TarDir

instance Pretty TarDir where
  pPrint (RealDir path) = text path
  pPrint (RegexDir regex) = text regex


-- | Where to fetch GHCupDownloads from.
data URLSource = GHCupURL
               | StackSetupURL
               | OwnSource     [Either (Either GHCupInfo SetupInfo) URI] -- ^ complete source list
               | OwnSpec               (Either GHCupInfo SetupInfo)
               | AddSource     [Either (Either GHCupInfo SetupInfo) URI] -- ^ merge with GHCupURL
               | SimpleList    [NewURLSource]
               deriving (Eq, GHC.Generic, Show)

data NewURLSource = NewGHCupURL
                  | NewStackSetupURL
                  | NewGHCupInfo     GHCupInfo
                  | NewSetupInfo     SetupInfo
                  | NewURI           URI
               deriving (Eq, GHC.Generic, Show)

instance NFData NewURLSource

fromURLSource :: URLSource -> [NewURLSource]
fromURLSource GHCupURL              = [NewGHCupURL]
fromURLSource StackSetupURL         = [NewStackSetupURL]
fromURLSource (OwnSource arr)       = convert' <$> arr
fromURLSource (AddSource arr)       = NewGHCupURL:(convert' <$> arr)
fromURLSource (SimpleList arr)      = arr
fromURLSource (OwnSpec (Left gi))   = [NewGHCupInfo gi]
fromURLSource (OwnSpec (Right si)) = [NewSetupInfo si]

convert' :: Either (Either GHCupInfo SetupInfo) URI -> NewURLSource
convert' (Left (Left gi))  = NewGHCupInfo gi
convert' (Left (Right si)) = NewSetupInfo si
convert' (Right uri)       = NewURI uri

instance NFData URLSource
instance NFData (URIRef Absolute) where
  rnf (URI !_ !_ !_ !_ !_) = ()


data MetaMode = Strict
              | Lax
  deriving (Show, Read, Eq, GHC.Generic)

instance NFData MetaMode

data UserSettings = UserSettings
  { uCache             :: Maybe Bool
  , uMetaCache         :: Maybe Integer
  , uMetaMode          :: Maybe MetaMode
  , uNoVerify          :: Maybe Bool
  , uVerbose           :: Maybe Bool
  , uKeepDirs          :: Maybe KeepDirs
  , uDownloader        :: Maybe Downloader
  , uKeyBindings       :: Maybe UserKeyBindings
  , uUrlSource         :: Maybe URLSource
  , uNoNetwork         :: Maybe Bool
  , uGPGSetting        :: Maybe GPGSetting
  , uPlatformOverride  :: Maybe PlatformRequest
  , uMirrors           :: Maybe DownloadMirrors
  , uDefGHCConfOptions :: Maybe [String]
  , uPager             :: Maybe PagerConfig
  }
  deriving (Show, GHC.Generic, Eq)

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fromSettings :: Settings -> Maybe KeyBindings -> UserSettings
fromSettings Settings{..} Nothing =
  UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uMetaMode = Just metaMode
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Nothing
    , uUrlSource = Just urlSource
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
    , uMirrors = Just mirrors
    , uDefGHCConfOptions = Just defGHCConfOptions
    , uPager = Just pager
  }
fromSettings Settings{..} (Just KeyBindings{..}) =
  let ukb = UserKeyBindings
            { kUp           = Just bUp
            , kDown         = Just bDown
            , kQuit         = Just bQuit
            , kInstall      = Just bInstall
            , kUninstall    = Just bUninstall
            , kSet          = Just bSet
            , kChangelog    = Just bChangelog
            , kShowAll      = Just bShowAllVersions
            }
  in UserSettings {
      uCache = Just cache
    , uMetaCache = Just metaCache
    , uMetaMode = Just metaMode
    , uNoVerify = Just noVerify
    , uVerbose = Just verbose
    , uKeepDirs = Just keepDirs
    , uDownloader = Just downloader
    , uNoNetwork = Just noNetwork
    , uKeyBindings = Just ukb
    , uUrlSource = Just urlSource
    , uGPGSetting = Just gpgSetting
    , uPlatformOverride = platformOverride
    , uMirrors = Just mirrors
    , uDefGHCConfOptions = Just defGHCConfOptions
    , uPager = Just pager
  }

data UserKeyBindings = UserKeyBindings
  { kUp           :: Maybe KeyCombination
  , kDown         :: Maybe KeyCombination
  , kQuit         :: Maybe KeyCombination
  , kInstall      :: Maybe KeyCombination
  , kUninstall    :: Maybe KeyCombination
  , kSet          :: Maybe KeyCombination
  , kChangelog    :: Maybe KeyCombination
  , kShowAll      :: Maybe KeyCombination
  }
  deriving (Show, GHC.Generic, Eq)

data KeyBindings = KeyBindings
  { bUp              :: KeyCombination
  , bDown            :: KeyCombination
  , bQuit            :: KeyCombination
  , bInstall         :: KeyCombination
  , bUninstall       :: KeyCombination
  , bSet             :: KeyCombination
  , bChangelog       :: KeyCombination
  , bShowAllVersions :: KeyCombination
  }
  deriving (Show, GHC.Generic)

instance NFData KeyBindings
#if !defined(BRICK)
instance NFData Key

instance NFData Modifier

#endif
instance NFData KeyCombination

defaultKeyBindings :: KeyBindings
defaultKeyBindings = KeyBindings
  { bUp              = KeyCombination { key = KUp      , mods = [] }
  , bDown            = KeyCombination { key = KDown    , mods = [] }
  , bQuit            = KeyCombination { key = KChar 'q', mods = [] }
  , bInstall         = KeyCombination { key = KChar 'i', mods = [] }
  , bUninstall       = KeyCombination { key = KChar 'u', mods = [] }
  , bSet             = KeyCombination { key = KChar 's', mods = [] }
  , bChangelog       = KeyCombination { key = KChar 'c', mods = [] }
  , bShowAllVersions = KeyCombination { key = KChar 'a', mods = [] }
  }

data AppState = AppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , ghcupInfo :: GHCupInfo
  , pfreq :: PlatformRequest
  , loggerConfig :: LoggerConfig
  } deriving (Show, GHC.Generic)

instance NFData AppState

fromAppState :: AppState -> LeanAppState
fromAppState AppState {..} = LeanAppState {..}

data LeanAppState = LeanAppState
  { settings :: Settings
  , dirs :: Dirs
  , keyBindings :: KeyBindings
  , loggerConfig :: LoggerConfig
  } deriving (Show, GHC.Generic)

instance NFData LeanAppState


data Settings = Settings
  { cache             :: Bool
  , metaCache         :: Integer
  , metaMode          :: MetaMode
  , noVerify          :: Bool
  , keepDirs          :: KeepDirs
  , downloader        :: Downloader
  , verbose           :: Bool
  , urlSource         :: URLSource
  , noNetwork         :: Bool
  , gpgSetting        :: GPGSetting
  , noColor           :: Bool -- this also exists in LoggerConfig
  , platformOverride  :: Maybe PlatformRequest
  , mirrors           :: DownloadMirrors
  , defGHCConfOptions :: [String]
  , pager             :: PagerConfig
  }
  deriving (Show, GHC.Generic)

data PagerConfig = PagerConfig {
    pagerList :: Bool
  , pagerCmd  :: Maybe String
  }
  deriving (Show, GHC.Generic, Eq)

instance NFData PagerConfig

defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig False Nothing

allPagerConfig :: String -> PagerConfig
allPagerConfig cmd = PagerConfig True (Just cmd)

defaultMetaCache :: Integer
defaultMetaCache = 300 -- 5 minutes

defaultSettings :: Settings
defaultSettings = Settings False defaultMetaCache Lax False Never Curl False GHCupURL False GPGNone False Nothing (DM mempty) [] defaultPagerConfig

instance NFData Settings

data Dirs = Dirs
  { baseDir    :: GHCupPath
  , binDir     :: FilePath
  , cacheDir   :: GHCupPath
  , logsDir    :: GHCupPath
  , confDir    :: GHCupPath
  , dbDir      :: GHCupPath
  , recycleDir :: GHCupPath -- mainly used on windows
  , tmpDir     :: GHCupPath
  , msys2Dir   :: FilePath
  }
  deriving (Show, GHC.Generic)

instance NFData Dirs

data MSYS2Env = MSYS
              | UCRT64
              | CLANG64
              | CLANGARM64
              | CLANG32
              | MINGW64
              | MINGW32
  deriving (Eq, Show, Ord, GHC.Generic, Read)

instance NFData MSYS2Env

data KeepDirs = Always
              | Errors
              | Never
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData KeepDirs

data Downloader = Curl
                | Wget
#if defined(INTERNAL_DOWNLOADER)
                | Internal
#endif
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData Downloader

data GPGSetting = GPGStrict
                | GPGLax
                | GPGNone
  deriving (Eq, Show, Ord, GHC.Generic)

instance NFData GPGSetting

data DebugInfo = DebugInfo
  { diBaseDir  :: FilePath
  , diBinDir   :: FilePath
  , diGHCDir   :: FilePath
  , diCacheDir :: FilePath
  , diArch     :: Architecture
  , diPlatform :: PlatformResult
  }
  deriving Show


data SetGHC = SetGHCOnly  -- ^ unversioned 'ghc'
            | SetGHC_XY   -- ^ ghc-x.y
            | SetGHC_XYZ  -- ^ ghc-x.y.z
            deriving (Eq, Show)

data SetHLS = SetHLSOnly  -- ^ unversioned 'hls'
            | SetHLS_XYZ  -- ^ haskell-language-server-a.b.c~x.y.z, where a.b.c is GHC version and x.y.z is HLS version
            deriving (Eq, Show)


data PlatformResult = PlatformResult
  { _platform      :: Platform
  , _distroVersion :: Maybe Versioning
  }
  deriving (Eq, Show, GHC.Generic)

instance NFData PlatformResult

platResToString :: PlatformResult -> String
platResToString PlatformResult { _platform = plat, _distroVersion = Just v' }
  = show plat <> ", " <> T.unpack (prettyV v')
platResToString PlatformResult { _platform = plat, _distroVersion = Nothing }
  = show plat

instance Pretty PlatformResult where
  pPrint = text . platResToString

data PlatformRequest = PlatformRequest
  { _rArch     :: Architecture
  , _rPlatform :: Platform
  , _rVersion  :: Maybe Versioning
  }
  deriving (Eq, Show, GHC.Generic)

instance NFData PlatformRequest

pfReqToString :: PlatformRequest -> String
pfReqToString (PlatformRequest arch plat ver) =
  archToString arch ++ "-" ++ platformToString plat ++ pver
 where
  pver = case ver of
           Just v' -> "-" ++ T.unpack (prettyV v')
           Nothing -> ""

instance Pretty PlatformRequest where
  pPrint = text . pfReqToString

-- | A GHC identified by the target platform triple
-- and the version.
data GHCTargetVersion = GHCTargetVersion
  { _tvTarget  :: Maybe Text
  , _tvVersion :: Version
  }
  deriving (Ord, Eq, Show, GHC.Generic)

instance NFData GHCTargetVersion

data GitBranch = GitBranch
  { ref  :: String
  , repo :: Maybe String
  }
  deriving (Ord, Eq, Show)

mkTVer :: Version -> GHCTargetVersion
mkTVer = GHCTargetVersion Nothing

tVerToText :: GHCTargetVersion -> Text
tVerToText (GHCTargetVersion (Just t) v') = t <> "-" <> prettyVer v'
tVerToText (GHCTargetVersion Nothing  v') = prettyVer v'

-- | Assembles a path of the form: <target-triple>-<version>
instance Pretty GHCTargetVersion where
  pPrint = text . T.unpack . tVerToText


-- | A comparator and a version.
data VersionCmp = VR_gt Versioning
                | VR_gteq Versioning
                | VR_lt Versioning
                | VR_lteq Versioning
                | VR_eq Versioning
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionCmp


-- | A version range. Supports && and ||, but not  arbitrary
-- combinations. This is a little simplified.
data VersionRange = SimpleRange (NonEmpty VersionCmp) -- And
                  | OrRange (NonEmpty VersionCmp) VersionRange
  deriving (Eq, GHC.Generic, Ord, Show)

instance NFData VersionRange

instance Pretty VersionCmp where
  pPrint (VR_gt v) = text "> " <> pPrint v
  pPrint (VR_gteq v) = text ">= " <> pPrint v
  pPrint (VR_lt v) = text "< " <> pPrint v
  pPrint (VR_lteq v) = text "<= " <> pPrint v
  pPrint (VR_eq v) = text "= " <> pPrint v

instance Pretty VersionRange where
  pPrint (SimpleRange xs) = foldl1 (\x y -> x <> text " && " <> y) $ NE.map pPrint xs
  pPrint (OrRange xs vr) = foldMap pPrint xs <> " || " <> pPrint vr

instance Pretty Versioning where
  pPrint = text . T.unpack . prettyV

instance Pretty Version where
  pPrint = text . T.unpack . prettyVer

instance Show (a -> b) where
  show _ = "<function>"

instance Show (IO ()) where
  show _ = "<io>"


data LogLevel = Warn
              | Info
              | Debug
              | Error
  deriving (Eq, Ord, Show)

data LoggerConfig = LoggerConfig
  { lcPrintDebug   :: Bool            -- ^ whether to print debug in colorOutter
  , consoleOutter  :: T.Text -> IO () -- ^ how to write the console output
  , fileOutter     :: T.Text -> IO () -- ^ how to write the file output
  , fancyColors    :: Bool
  }
  deriving Show

instance NFData LoggerConfig where
  rnf (LoggerConfig !lcPrintDebug !_ !_ !fancyColors) = rnf (lcPrintDebug, fancyColors)

data ProcessError = NonZeroExit Int FilePath [String]
                  | PTerminated FilePath [String]
                  | PStopped FilePath [String]
                  | NoSuchPid FilePath [String]
                  deriving Show


data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut   :: BL.ByteString
  , _stdErr   :: BL.ByteString
  }
  deriving (Eq, Show)

makeLenses ''CapturedProcess


data InstallDir = IsolateDir FilePath
                | GHCupInternal
  deriving (Eq, Show)

data InstallDirResolved = IsolateDirResolved FilePath
                        | GHCupDir GHCupPath
                        | GHCupBinDir FilePath
  deriving (Eq, Show)

fromInstallDir :: InstallDirResolved -> FilePath
fromInstallDir (IsolateDirResolved fp) = fp
fromInstallDir (GHCupDir fp) = fromGHCupPath fp
fromInstallDir (GHCupBinDir fp) = fp


isSafeDir :: InstallDirResolved -> Bool
isSafeDir (IsolateDirResolved _) = False
isSafeDir (GHCupDir _)           = True
isSafeDir (GHCupBinDir _)        = False

type PromptQuestion = Text

data PromptResponse = PromptYes | PromptNo
  deriving (Show, Eq)

data ToolVersion = GHCVersion GHCTargetVersion
                 | ToolVersion Version
                 | ToolTag Tag
                 | ToolDay Day
                 deriving (Eq, Show)

instance Pretty ToolVersion where
  pPrint (GHCVersion v) = pPrint v
  pPrint (ToolVersion v) = pPrint v
  pPrint (ToolTag t) = pPrint t
  pPrint (ToolDay d) = text (show d)



data BuildSystem = Hadrian
                 | Make
  deriving (Show, Eq)


data VersionPattern = CabalVer
                    | GitHashShort
                    | GitHashLong
                    | GitDescribe
                    | GitBranchName
                    | S String
  deriving (Eq, Show)

