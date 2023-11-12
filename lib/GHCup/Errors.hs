{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE RankNTypes           #-}

{-|
Module      : GHCup.Errors
Description : GHCup error types
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Errors where

import           GHCup.Types

import           Codec.Archive
import           Control.Exception.Safe
import           Data.ByteString                ( ByteString )
import           Data.CaseInsensitive           ( CI )
import           Data.Text                      ( Text )
import           Data.Versions
import           Haskus.Utils.Variant
import           System.FilePath
import           Text.PrettyPrint               hiding ( (<>) )
import           Text.PrettyPrint.HughesPJClass hiding ( (<>) )
import           URI.ByteString

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Encoding.Error      as E
import           Data.Data (Proxy(..))
import Data.Time (Day)



allHFError :: String
allHFError = unlines allErrors
 where
  format p = "GHCup-" <> show (eBase p) <> " " <>  eDesc p
  format'' e p = "GHCup-" <> show (eNum e) <> " " <>  eDesc p
  format' e  _ = "GHCup-" <> show (eNum e) <> " " <>  prettyShow e
  format''' e  _ str' = "GHCup-" <> show (eNum e) <> " " <>  str'
  allErrors =
    [ "# low level errors (1 to 500)"
    , let proxy = Proxy :: Proxy NoCompatiblePlatform in format proxy
    , let proxy = Proxy :: Proxy NoDownload in format proxy
    , let proxy = Proxy :: Proxy NoUpdate in format proxy
    , let proxy = Proxy :: Proxy DistroNotFound in format proxy
    , let proxy = Proxy :: Proxy UnknownArchive in format proxy
    , let proxy = Proxy :: Proxy UnsupportedScheme in format proxy
    , let proxy = Proxy :: Proxy CopyError in format proxy
    , let proxy = Proxy :: Proxy MergeFileTreeError in format proxy
    , let proxy = Proxy :: Proxy TagNotFound in format proxy
    , let proxy = Proxy :: Proxy DayNotFound in format proxy
    , let proxy = Proxy :: Proxy NextVerNotFound in format proxy
    , let proxy = Proxy :: Proxy AlreadyInstalled in format proxy
    , let proxy = Proxy :: Proxy DirNotEmpty in format proxy
    , let proxy = Proxy :: Proxy NotInstalled in format proxy
    , let proxy = Proxy :: Proxy UninstallFailed in format proxy
    , let proxy = Proxy :: Proxy NotFoundInPATH in format proxy
    , let proxy = Proxy :: Proxy JSONError in format proxy
    , let proxy = Proxy :: Proxy FileDoesNotExistError in format proxy
    , let proxy = Proxy :: Proxy FileAlreadyExistsError in format proxy
    , let proxy = Proxy :: Proxy TarDirDoesNotExist in format proxy
    , let proxy = Proxy :: Proxy DigestError in format proxy
    , let proxy = Proxy :: Proxy GPGError in format proxy
    , let proxy = Proxy :: Proxy HTTPStatusError in format proxy
    , let proxy = Proxy :: Proxy MalformedHeaders in format proxy
    , let proxy = Proxy :: Proxy HTTPNotModified in format proxy
    , let proxy = Proxy :: Proxy NoLocationHeader in format proxy
    , let proxy = Proxy :: Proxy TooManyRedirs in format proxy
    , let proxy = Proxy :: Proxy PatchFailed in format proxy
    , let proxy = Proxy :: Proxy NoToolRequirements in format proxy
    , let proxy = Proxy :: Proxy InvalidBuildConfig in format proxy
    , let proxy = Proxy :: Proxy NoToolVersionSet in format proxy
    , let proxy = Proxy :: Proxy NoNetwork in format proxy
    , let proxy = Proxy :: Proxy HadrianNotFound in format proxy
    , let proxy = Proxy :: Proxy ToolShadowed in format proxy
    , let proxy = Proxy :: Proxy ContentLengthError in format proxy
    , let proxy = Proxy :: Proxy DuplicateReleaseChannel in format proxy
    , let proxy = Proxy :: Proxy UnsupportedSetupCombo in format proxy
    , ""
    , "# high level errors (4000+)"
    , let proxy = Proxy :: Proxy DownloadFailed in format proxy
    , let proxy = Proxy :: Proxy InstallSetError in format proxy
    , let proxy = Proxy :: Proxy TestFailed in format proxy
    , let proxy = Proxy :: Proxy BuildFailed in format proxy
    , let proxy = Proxy :: Proxy GHCupSetError in format proxy
    , ""
    , "# true exceptions (500+)"
    , let proxy = Proxy :: Proxy ParseError in format proxy
    , let proxy = Proxy :: Proxy UnexpectedListLength in format proxy
    , let proxy = Proxy :: Proxy NoUrlBase in format proxy
    , let proxy = Proxy :: Proxy DigestMissing in format proxy
    , ""
    , "# orphans (800+)"
    , let proxy = Proxy :: Proxy URIParseError in format proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedScheme MissingColon
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedUserInfo
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedQuery
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedFragment
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedHost
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedPort
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = MalformedPath
      in format' e proxy
    , let proxy = Proxy :: Proxy URIParseError
          e     = OtherError ""
      in format'' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult in format proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveFatal
      in format' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveFailed
      in format' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveWarn
      in format' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveRetry
      in format' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveOk
      in format' e proxy
    , let proxy = Proxy :: Proxy ArchiveResult
          e     = ArchiveEOF
      in format' e proxy

    , let proxy = Proxy :: Proxy ProcessError in format proxy
    , let proxy = Proxy :: Proxy ProcessError
          e     = NonZeroExit 0 "" []
      in format''' e proxy "A process returned a non-zero exit code."
    , let proxy = Proxy :: Proxy ProcessError
          e     = PTerminated "" []
      in format''' e proxy "A process terminated prematurely."
    , let proxy = Proxy :: Proxy ProcessError
          e     = PStopped "" []
      in format''' e proxy "A process stopped prematurely."
    , let proxy = Proxy :: Proxy ProcessError
          e     = NoSuchPid "" []
      in format''' e proxy "Could not find PID for this process."
    ]


prettyHFError :: (Pretty e, HFErrorProject e) => e -> String
prettyHFError e =
  let errorCode = "GHCup-" <> padIntAndShow (eNum e)
  in ("[" <> linkEscapeCode errorCode  (hfErrorLink errorCode) <> "] ") <> prettyShow e
 where
  hfErrorLink errorCode = "https://errors.haskell.org/messages/" <> errorCode
  padIntAndShow i
    | i < 10    = "0000" <> show i
    | i < 100   = "000"  <> show i
    | i < 1000  = "00"   <> show i
    | i < 10000 = "0"    <> show i
    | otherwise =           show i

class HFErrorProject a where
  eNum :: a -> Int
  eNum _ = eBase (Proxy :: Proxy a)

  eBase :: Proxy a -> Int

  eDesc :: Proxy a -> String

linkEscapeCode :: String -> String -> String
linkEscapeCode linkText link = "\ESC]8;;" <> link <> "\ESC\\" <> linkText <> "\ESC]8;;\ESC\\"


    ------------------------
    --[ Low-level errors ]--
    ------------------------



-- | A compatible platform could not be found.
data NoCompatiblePlatform = NoCompatiblePlatform String -- the platform we got
  deriving Show

instance Pretty NoCompatiblePlatform where
  pPrint (NoCompatiblePlatform str') =
    text ("Could not find a compatible platform. Got: " ++ str')

instance HFErrorProject NoCompatiblePlatform where
  eBase _ = 1
  eDesc _ = "No compatible platform could be found"

-- | Unable to find a download for the requested version/distro.
data NoDownload = NoDownload GHCTargetVersion Tool (Maybe PlatformRequest)
                | NoDownload' GlobalTool
  deriving Show

instance Pretty NoDownload where
  pPrint (NoDownload tver@(GHCTargetVersion mtarget vv) tool mpfreq)
    | (Just target) <- mtarget
    , target `elem` (T.pack . prettyShow <$> enumFromTo (minBound :: Tool) (maxBound :: Tool))
    = text $ "Unable to find a download for "
             <> show tool
             <> " version '"
             <> T.unpack (tVerToText tver)
             <> maybe "'\n" (\pfreq -> "' on detected platform " <> pfReqToString pfreq <> "\n") mpfreq
             <> "Perhaps you meant: 'ghcup <command> "
             <> T.unpack target
             <> " "
             <> T.unpack (prettyVer vv)
             <> "'"
    | otherwise = text $ "Unable to find a download for " <> T.unpack (tVerToText tver)
  pPrint (NoDownload' globalTool) = text $ "Unable to find a download for " <> prettyShow globalTool

instance HFErrorProject NoDownload where
  eBase _ = 10
  eDesc _ = "Unable to find a download for the requested version/distro."

-- | No update available or necessary.
data NoUpdate = NoUpdate
  deriving Show

instance Pretty NoUpdate where
  pPrint NoUpdate = text (eDesc (Proxy :: Proxy NoUpdate))

instance HFErrorProject NoUpdate where
  eBase _ = 20
  eDesc _ = "No update available or necessary."

-- | The Architecture is unknown and unsupported.
data NoCompatibleArch = NoCompatibleArch String
  deriving Show

instance Pretty NoCompatibleArch where
  pPrint (NoCompatibleArch arch) =
    text ("The Architecture is unknown or unsupported. Got: " ++ arch)

instance HFErrorProject NoCompatibleArch where
  eBase _ = 30
  eDesc _ = "The Architecture is unknown and unsupported"

-- | Unable to figure out the distribution of the host.
data DistroNotFound = DistroNotFound
  deriving Show

instance Pretty DistroNotFound where
  pPrint DistroNotFound =
    text (eDesc (Proxy :: Proxy DistroNotFound))

instance HFErrorProject DistroNotFound where
  eBase _ = 40
  eDesc _ = "Unable to figure out the distribution of the host"

-- | The archive format is unknown. We don't know how to extract it.
data UnknownArchive = UnknownArchive FilePath
  deriving Show

instance Pretty UnknownArchive where
  pPrint (UnknownArchive file) =
    text $ "The archive format is unknown. We don't know how to extract the file " <> file

instance HFErrorProject UnknownArchive where
  eBase _ = 50
  eDesc _ = "The archive format is unknown. We don't know how to extract it."

-- | The scheme is not supported (such as ftp).
data UnsupportedScheme = UnsupportedScheme
  deriving Show

instance Pretty UnsupportedScheme where
  pPrint UnsupportedScheme =
    text (eDesc (Proxy :: Proxy UnsupportedScheme))

instance HFErrorProject UnsupportedScheme where
  eBase _ = 60
  eDesc _ = "The scheme is not supported (such as ftp)."

-- | Unable to copy a file.
data CopyError = CopyError String
  deriving Show

instance Pretty CopyError where
  pPrint (CopyError reason) =
    text ("Unable to copy a file. Reason was: " ++ reason)

instance HFErrorProject CopyError where
  eBase _ = 70
  eDesc _ = "Unable to copy a file."

-- | Unable to merge file trees.
data MergeFileTreeError = MergeFileTreeError IOException FilePath FilePath
  deriving Show

instance Pretty MergeFileTreeError where
  pPrint (MergeFileTreeError e from to) =
    text "Failed to merge file tree from" <+> text from <+> text "to" <+> text to <+> text "\nexception was:" <+> text (displayException e)
     <+> text "\n...you may need to delete" <+> text to <+> text "manually. Make sure it's gone."

instance HFErrorProject MergeFileTreeError where
  eBase _ = 80
  eDesc _ = "Unable to merge file trees during installation"

-- | Unable to find a tag of a tool.
data TagNotFound = TagNotFound Tag Tool
  deriving Show

instance Pretty TagNotFound where
  pPrint (TagNotFound tag tool) =
    text "Unable to find tag" <+> pPrint tag <+> text "of tool" <+> pPrint tool

instance HFErrorProject TagNotFound where
  eBase _ = 90
  eDesc _ = "Unable to find a tag of a tool"

-- | Unable to find a release day of a tool
data DayNotFound = DayNotFound Day Tool (Maybe Day)
  deriving Show

instance Pretty DayNotFound where
  pPrint (DayNotFound day tool Nothing) =
    text "Unable to find release date" <+> text (show day) <+> text "of tool" <+> pPrint tool
  pPrint (DayNotFound day tool (Just alternateDay)) =
    text "Unable to find release date" <+> text (show day) <+> text "of tool" <+> pPrint tool <+>
      text "but found an alternative date" <+> text (show alternateDay)

instance HFErrorProject DayNotFound where
  eBase _ = 95
  eDesc _ = "Unable to find a release date of a tool"

-- | Unable to find the next version of a tool (the one after the currently
-- set one).
data NextVerNotFound = NextVerNotFound Tool
  deriving Show

instance Pretty NextVerNotFound where
  pPrint (NextVerNotFound tool) =
    text "Unable to find next (the one after the currently set one) version of tool" <+> pPrint tool

instance HFErrorProject NextVerNotFound where
  eBase _ = 100
  eDesc _ = "Unable to find the next version of a tool (the one after the currently set one)"

-- | The tool (such as GHC) is already installed with that version.
data AlreadyInstalled = AlreadyInstalled Tool Version
  deriving Show

instance Pretty AlreadyInstalled where
  pPrint (AlreadyInstalled tool ver') =
    (pPrint tool <> text "-" <> pPrint ver') <+> text "is already installed;"
    <+> text "if you really want to reinstall it, you may want to run 'ghcup install" <+> pPrint tool <+> text "--force" <+> (pPrint ver' <> text "'")

instance HFErrorProject AlreadyInstalled where
  eBase _ = 110
  eDesc _ = "The tool (such as GHC) is already installed with that version"

-- | The Directory is supposed to be empty, but wasn't.
data DirNotEmpty = DirNotEmpty {path :: FilePath}
  deriving Show

instance Pretty DirNotEmpty where
  pPrint (DirNotEmpty path) = do
    text $ "The directory was expected to be empty, but isn't: " <> path

instance HFErrorProject DirNotEmpty where
  eBase _ = 120
  eDesc _ = "The Directory is supposed to be empty, but wasn't"

-- | The tool is not installed. Some operations rely on a tool
-- to be installed (such as setting the current GHC version).
data NotInstalled = NotInstalled Tool GHCTargetVersion
  deriving Show

instance Pretty NotInstalled where
  pPrint (NotInstalled tool ver) =
    text "The version" <+> pPrint ver <+> text "of the tool" <+> pPrint tool <+> text "is not installed."

instance HFErrorProject NotInstalled where
  eBase _ = 130
  eDesc _ = "The required tool is not installed"

data UninstallFailed = UninstallFailed FilePath [FilePath]
  deriving Show

instance Pretty UninstallFailed where
  pPrint (UninstallFailed dir files) =
    text "The following files survived uninstallation: " <+> pPrint files <+> text "...consider removing" <+> pPrint dir <+> text "manually."

instance HFErrorProject UninstallFailed where
  eBase _ = 140
  eDesc _ = "Uninstallation failed with leftover files"

-- | An executable was expected to be in PATH, but was not found.
data NotFoundInPATH = NotFoundInPATH FilePath
  deriving Show

instance Exception NotFoundInPATH

instance Pretty NotFoundInPATH where
  pPrint (NotFoundInPATH exe) =
    text $ "The exe " <> exe <> " was not found in PATH."

instance HFErrorProject NotFoundInPATH where
  eBase _ = 150
  eDesc _ = "An executable was expected to be in PATH, but was not found"

-- | JSON decoding failed.
data JSONError = JSONDecodeError String
  deriving Show

instance Pretty JSONError where
  pPrint (JSONDecodeError err) =
    text $ "JSON decoding failed with: " <> err

instance HFErrorProject JSONError where
  eBase _ = 160
  eDesc _ = "JSON decoding failed"

-- | A file that is supposed to exist does not exist
-- (e.g. when we use file scheme to "download" something).
data FileDoesNotExistError = FileDoesNotExistError FilePath
  deriving Show

instance Pretty FileDoesNotExistError where
  pPrint (FileDoesNotExistError file) =
    text $ "File " <> file <> " does not exist."

instance HFErrorProject FileDoesNotExistError where
  eBase _ = 170
  eDesc _ = "A file that is supposed to exist does not exist (oops)"

-- | The file already exists
-- (e.g. when we use isolated installs with the same path).
-- (e.g. This is done to prevent any overwriting)
data FileAlreadyExistsError = FileAlreadyExistsError FilePath
  deriving Show

instance Pretty FileAlreadyExistsError where
  pPrint (FileAlreadyExistsError file) =
    text $ "File " <> file <> " Already exists."

instance HFErrorProject FileAlreadyExistsError where
  eBase _ = 180
  eDesc _ = "A file already exists that wasn't expected to exist"

data TarDirDoesNotExist = TarDirDoesNotExist TarDir
  deriving Show

instance Pretty TarDirDoesNotExist where
  pPrint (TarDirDoesNotExist dir) =
    text "Tar directory does not exist:" <+> pPrint dir

instance HFErrorProject TarDirDoesNotExist where
  eBase _ = 190
  eDesc _ = "The tar directory (e.g. inside an archive) does not exist"

-- | File digest verification failed.
data DigestError = DigestError FilePath Text Text
  deriving Show

instance Pretty DigestError where
  pPrint (DigestError fp currentDigest expectedDigest) =
    text "Digest error for" <+> text (fp <> ": expected")
      <+> text (T.unpack expectedDigest) <+> text "but got" <+> pPrint currentDigest <+> text
      "\nConsider removing the file in case it's cached and try again."

instance HFErrorProject DigestError where
  eBase _ = 200
  eDesc _ = "File digest verification failed"

-- | File PGP verification failed.
data GPGError = forall xs . (ToVariantMaybe DownloadFailed xs, PopVariant DownloadFailed xs, Show (V xs), Pretty (V xs)) => GPGError (V xs)

deriving instance Show GPGError

instance Pretty GPGError where
  pPrint (GPGError reason) = text "GPG verify failed:" <+> pPrint reason

instance HFErrorProject GPGError where
  eBase _ = 210
  eDesc _ = "File PGP verification failed"

-- | Unexpected HTTP status.
data HTTPStatusError = HTTPStatusError Int (M.Map (CI ByteString) ByteString)
  deriving Show

instance Pretty HTTPStatusError where
  pPrint (HTTPStatusError status _) =
    text "Unexpected HTTP status:" <+> pPrint status

instance HFErrorProject HTTPStatusError where
  eBase _ = 220
  eDesc _ = "Unexpected HTTP status error (e.g. during downloads)"

-- | Malformed headers.
data MalformedHeaders = MalformedHeaders Text
  deriving Show

instance Pretty MalformedHeaders where
  pPrint (MalformedHeaders h) =
    text "Headers are malformed: " <+> pPrint h

instance HFErrorProject MalformedHeaders where
  eBase _ = 230
  eDesc _ = "Malformed headers during download"

-- | Unexpected HTTP status.
data HTTPNotModified = HTTPNotModified Text
  deriving Show

instance Pretty HTTPNotModified where
  pPrint (HTTPNotModified etag) =
    text "Remote resource not modifed, etag was:" <+> pPrint etag

instance HFErrorProject HTTPNotModified where
  eBase _ = 240
  eDesc _ = "Not modified HTTP status error (e.g. during downloads)."

-- | The 'Location' header was expected during a 3xx redirect, but not found.
data NoLocationHeader = NoLocationHeader
  deriving Show

instance Pretty NoLocationHeader where
  pPrint NoLocationHeader =
    text (eDesc (Proxy :: Proxy NoLocationHeader))

instance HFErrorProject NoLocationHeader where
  eBase _ = 250
  eDesc _ = "The 'Location' header was expected during a 3xx redirect, but not found."

-- | Too many redirects.
data TooManyRedirs = TooManyRedirs
  deriving Show

instance Pretty TooManyRedirs where
  pPrint TooManyRedirs =
    text (eDesc (Proxy :: Proxy TooManyRedirs))

instance HFErrorProject TooManyRedirs where
  eBase _ = 260
  eDesc _ = "Too many redirections."

-- | A patch could not be applied.
data PatchFailed = PatchFailed
  deriving Show

instance Pretty PatchFailed where
  pPrint PatchFailed =
    text (eDesc (Proxy :: Proxy PatchFailed))

instance HFErrorProject PatchFailed where
  eBase _ = 270
  eDesc _ = "A patch could not be applied."

-- | The tool requirements could not be found.
data NoToolRequirements = NoToolRequirements
  deriving Show

instance Pretty NoToolRequirements where
  pPrint NoToolRequirements =
    text (eDesc (Proxy :: Proxy NoToolRequirements))

instance HFErrorProject NoToolRequirements where
  eBase _ = 280
  eDesc _ = "The Tool requirements could not be found."

data InvalidBuildConfig = InvalidBuildConfig Text
  deriving Show

instance Pretty InvalidBuildConfig where
  pPrint (InvalidBuildConfig reason) =
    text "The build config is invalid. Reason was:" <+> pPrint reason

instance HFErrorProject InvalidBuildConfig where
  eBase _ = 290
  eDesc _ = "The build config is invalid."

data NoToolVersionSet = NoToolVersionSet Tool
  deriving Show

instance Pretty NoToolVersionSet where
  pPrint (NoToolVersionSet tool) =
    text "No version is set for tool" <+> pPrint tool <+> text "."

instance HFErrorProject NoToolVersionSet where
  eBase _ = 300
  eDesc _ = "No version is set for tool (but was expected)."

data NoNetwork = NoNetwork
  deriving Show

instance Pretty NoNetwork where
  pPrint NoNetwork =
    text (eDesc (Proxy :: Proxy NoNetwork))

instance HFErrorProject NoNetwork where
  eBase _ = 310
  eDesc _ = "A download was required or requested, but '--offline' was specified."

data HadrianNotFound = HadrianNotFound
  deriving Show

instance Pretty HadrianNotFound where
  pPrint HadrianNotFound =
    text (eDesc (Proxy :: Proxy HadrianNotFound))

instance HFErrorProject HadrianNotFound where
  eBase _ = 320
  eDesc _ = "Could not find Hadrian build files. Does this GHC version support Hadrian builds?"

data ToolShadowed = ToolShadowed
                       Tool
                       FilePath  -- shadow binary
                       FilePath  -- upgraded binary
                       Version   -- upgraded version
  deriving Show

instance Pretty ToolShadowed where
  pPrint (ToolShadowed tool sh up _) =
    text (prettyShow tool
         <> " is shadowed by "
         <> sh
         <> ".\nThe upgrade will not be in effect, unless you remove "
         <> sh
         <> "\nor make sure "
         <> takeDirectory up
         <> " comes before "
         <> takeDirectory sh
         <> " in PATH."
         )

instance HFErrorProject ToolShadowed where
  eBase _ = 330
  eDesc _ = "A tool is shadowed in PATH."

-- | File content length verification failed.
data ContentLengthError = ContentLengthError (Maybe FilePath) (Maybe Integer) Integer
  deriving Show

instance Pretty ContentLengthError where
  pPrint (ContentLengthError Nothing Nothing expectedSize) =
    text "Content length exceeded expected size:"
      <+> text (show expectedSize)
      <+> text "\nConsider removing the file in case it's cached and try again."
  pPrint (ContentLengthError Nothing (Just currentSize) expectedSize) =
    text "Content length error. Expected"
      <+> text (show expectedSize) <+> text "but got" <+> pPrint currentSize <+> text
      "\nConsider removing the file in case it's cached and try again."
  pPrint (ContentLengthError (Just fp) (Just currentSize) expectedSize) =
    text "Content length error for" <+> text (fp <> ": expected")
      <+> text (show expectedSize) <+> text "but got" <+> pPrint currentSize <+> text
      "\nConsider removing the file in case it's cached and try again."
  pPrint (ContentLengthError (Just fp) Nothing expectedSize) =
    text "Content length error for" <+> text (fp <> ": expected")
      <+> text (show expectedSize) <+> text "\nConsider removing the file in case it's cached and try again."

instance Exception ContentLengthError

instance HFErrorProject ContentLengthError where
  eBase _ = 340
  eDesc _ = "File content length verification failed"

data DuplicateReleaseChannel = DuplicateReleaseChannel NewURLSource
  deriving Show

instance HFErrorProject DuplicateReleaseChannel where
  eBase _ = 350
  eDesc _ = "Duplicate release channel detected when adding new source.\nGiving up. You can use '--force' to remove and append the duplicate source (this may change order/semantics)."

instance Pretty DuplicateReleaseChannel where
  pPrint (DuplicateReleaseChannel source) =
    text $ "Duplicate release channel detected when adding: \n  "
      <> show source
      <> "\nGiving up. You can use '--force' to remove and append the duplicate source (this may change order/semantics)."

data UnsupportedSetupCombo = UnsupportedSetupCombo Architecture Platform
  deriving Show

instance Pretty UnsupportedSetupCombo where
  pPrint (UnsupportedSetupCombo arch plat) =
    text "Could not find a compatible setup combo for:" <+> pPrint arch <+> pPrint plat

instance HFErrorProject UnsupportedSetupCombo where
  eBase _ = 360
  eDesc _ = "Could not find a compatible setup combo"

    -------------------------
    --[ High-level errors ]--
    -------------------------

-- | A download failed. The underlying error is encapsulated.
data DownloadFailed = forall xs . (HFErrorProject (V xs), ToVariantMaybe DownloadFailed xs, PopVariant DownloadFailed xs, Show (V xs), Pretty (V xs)) => DownloadFailed (V xs)

instance Pretty DownloadFailed where
  pPrint (DownloadFailed reason) =
    case reason of
      VMaybe (_ :: DownloadFailed) -> pPrint reason
      _ -> text "Download failed:" <+> pPrint reason

deriving instance Show DownloadFailed

instance HFErrorProject DownloadFailed where
  eBase _ = 5000
  eNum (DownloadFailed xs) = 5000 + eNum xs
  eDesc _ = "A download failed."

data InstallSetError = forall xs1 xs2 . (Show (V xs1), Pretty (V xs1), HFErrorProject (V xs1), Show (V xs2), Pretty (V xs2), HFErrorProject (V xs2)) => InstallSetError (V xs1) (V xs2)

instance Pretty InstallSetError where
  pPrint (InstallSetError reason1 reason2) =
     text "Both installation and setting the tool failed.\nInstall error was:"
      <+> pPrint reason1
      <+> text "\nSet error was:"
      <+> pPrint reason2

deriving instance Show InstallSetError

instance HFErrorProject InstallSetError where
  eBase _ = 7000
  -- will there be collisions?
  eNum (InstallSetError xs1 xs2) = 7000 + eNum xs1 + eNum xs2
  eDesc _ = "Installation or setting the tool failed."


-- | A test failed.
data TestFailed = forall es . (ToVariantMaybe TestFailed es, PopVariant TestFailed es, Pretty (V es), Show (V es), HFErrorProject (V es)) => TestFailed FilePath (V es)

instance Pretty TestFailed where
  pPrint (TestFailed path reason) =
    case reason of
      VMaybe (_ :: TestFailed) -> pPrint reason
      _ -> text ("The test failed. GHC test suite is fragile and non-portable. Please also check out the " <> linkEscapeCode "issue tracker" " https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=updated_desc&state=opened&label_name%5B%5D=testsuite&label_name%5B%5D=packaging&first_page_size=20" <> ".\nBuild dir was:") <+> text path <+> text "\nReason was:" <+> pPrint reason

deriving instance Show TestFailed

instance HFErrorProject TestFailed where
  eBase _ = 4000
  eNum (TestFailed _ xs2) = 4000 + eNum xs2
  eDesc _ = "The test failed."

-- | A build failed.
data BuildFailed = forall es . (ToVariantMaybe BuildFailed es, PopVariant BuildFailed es, Pretty (V es), Show (V es), HFErrorProject (V es)) => BuildFailed FilePath (V es)

instance Pretty BuildFailed where
  pPrint (BuildFailed path reason) =
    case reason of
      VMaybe (_ :: BuildFailed) -> pPrint reason
      _ -> text "BuildFailed failed in dir" <+> text (path <> ":") <+> pPrint reason

deriving instance Show BuildFailed

instance HFErrorProject BuildFailed where
  eBase _ = 8000
  eNum (BuildFailed _ xs2) = 8000 + eNum xs2
  eDesc _ = "The build failed."


-- | Setting the current GHC version failed.
data GHCupSetError = forall es . (ToVariantMaybe GHCupSetError es, PopVariant GHCupSetError es, Show (V es), Pretty (V es), HFErrorProject (V es)) => GHCupSetError (V es)

instance Pretty GHCupSetError where
  pPrint (GHCupSetError reason) =
    case reason of
      VMaybe (_ :: GHCupSetError) -> pPrint reason
      _ -> text "Setting the current version failed:" <+> pPrint reason

deriving instance Show GHCupSetError

instance HFErrorProject GHCupSetError where
  eBase _ = 9000
  eNum (GHCupSetError xs) = 9000 + eNum xs
  eDesc _ = "Setting the current version failed."

-- | Executing stacks platform detection failed.
data StackPlatformDetectError = forall es . (ToVariantMaybe StackPlatformDetectError es, PopVariant StackPlatformDetectError es, Show (V es), Pretty (V es), HFErrorProject (V es)) => StackPlatformDetectError (V es)

instance Pretty StackPlatformDetectError where
  pPrint (StackPlatformDetectError reason) =
    case reason of
      VMaybe (_ :: StackPlatformDetectError) -> pPrint reason
      _ -> text "Running stack platform detection logic failed:" <+> pPrint reason

deriving instance Show StackPlatformDetectError

instance HFErrorProject StackPlatformDetectError where
  eBase _ = 6000
  eNum (StackPlatformDetectError xs) = 6000 + eNum xs
  eDesc _ = "Running stack platform detection logic failed."


    ---------------------------------------------
    --[ True Exceptions (e.g. for MonadThrow) ]--
    ---------------------------------------------


-- | Parsing failed.
data ParseError = ParseError String
  deriving Show

instance Pretty ParseError where
  pPrint (ParseError reason) =
    text "Parsing failed:" <+> pPrint reason

instance Exception ParseError

instance HFErrorProject ParseError where
  eBase _ = 500
  eDesc _ = "A parse error occured."


data UnexpectedListLength = UnexpectedListLength String
  deriving Show

instance Pretty UnexpectedListLength where
  pPrint (UnexpectedListLength reason) =
    text "List length unexpected:" <+> pPrint reason

instance Exception UnexpectedListLength

instance HFErrorProject UnexpectedListLength where
  eBase _ = 510
  eDesc _ = "A list had an unexpected length."

data NoUrlBase = NoUrlBase Text
  deriving Show

instance Pretty NoUrlBase where
  pPrint (NoUrlBase url) =
    text "Couldn't get a base filename from url" <+> pPrint url

instance Exception NoUrlBase

instance HFErrorProject NoUrlBase where
  eBase _ = 520
  eDesc _ = "URL does not have a base filename."

data DigestMissing = DigestMissing URI
  deriving Show

instance Pretty DigestMissing where
  pPrint (DigestMissing uri) =
    text "Digest missing for:" <+> (text . T.unpack . E.decodeUtf8With E.lenientDecode . serializeURIRef') uri

instance Exception DigestMissing

instance HFErrorProject DigestMissing where
  eBase _ = 530
  eDesc _ = "An expected digest is missing."


    ------------------------
    --[ orphan instances ]--
    ------------------------

instance Pretty (V '[]) where
   {-# INLINABLE pPrint #-}
   pPrint _ = undefined

instance
   ( Pretty x
   , Pretty (V xs)
   ) => Pretty (V (x ': xs))
   where
      pPrint v = case popVariantHead v of
         Right x -> pPrint x
         Left xs -> pPrint xs

instance HFErrorProject (V '[]) where
   {-# INLINABLE eBase #-}
   eBase _ = undefined
   {-# INLINABLE eDesc #-}
   eDesc _ = undefined

instance
   ( HFErrorProject x
   , HFErrorProject (V xs)
   ) => HFErrorProject (V (x ': xs))
   where
      eNum v = case popVariantHead v of
         Right x -> eNum x
         Left xs -> eNum xs
      eDesc _ = undefined
      eBase _ = undefined

instance Pretty URIParseError where
  pPrint (MalformedScheme reason) =
    text "Failed to parse URI. Malformed scheme:" <+> text (show reason)
  pPrint MalformedUserInfo =
    text "Failed to parse URI. Malformed user info."
  pPrint MalformedQuery =
    text "Failed to parse URI. Malformed query."
  pPrint MalformedFragment =
    text "Failed to parse URI. Malformed fragment."
  pPrint MalformedHost =
    text "Failed to parse URI. Malformed host."
  pPrint MalformedPort =
    text "Failed to parse URI. Malformed port."
  pPrint MalformedPath =
    text "Failed to parse URI. Malformed path."
  pPrint (OtherError err) =
    text "Failed to parse URI:" <+> pPrint err

instance HFErrorProject URIParseError where
  eBase _ = 800

  eNum (MalformedScheme NonAlphaLeading) = 801
  eNum (MalformedScheme InvalidChars) = 802
  eNum (MalformedScheme MissingColon) = 803
  eNum MalformedUserInfo   = 804
  eNum MalformedQuery      = 805
  eNum MalformedFragment   = 806
  eNum MalformedHost       = 807
  eNum MalformedPort       = 808
  eNum MalformedPath       = 809
  eNum (OtherError _)      = 810

  eDesc _ = "Failed to parse URI."

instance Pretty ArchiveResult where
  pPrint ArchiveFatal = text "Archive result: fatal"
  pPrint ArchiveFailed = text "Archive result: failed"
  pPrint ArchiveWarn = text "Archive result: warning"
  pPrint ArchiveRetry = text "Archive result: retry"
  pPrint ArchiveOk = text "Archive result: Ok"
  pPrint ArchiveEOF = text "Archive result: EOF"

instance HFErrorProject ArchiveResult where
  eBase _ = 820

  eNum ArchiveFatal  = 821
  eNum ArchiveFailed = 822
  eNum ArchiveWarn   = 823
  eNum ArchiveRetry  = 824
  eNum ArchiveOk     = 825
  eNum ArchiveEOF    = 826

  eDesc _ = "Archive extraction result."

instance Pretty T.Text where
  pPrint = text . T.unpack

instance Pretty ProcessError where
  pPrint (NonZeroExit e exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "failed with exit code" <+> text (show e <> ".")
  pPrint (PTerminated exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "terminated."
  pPrint (PStopped exe args) =
    text "Process" <+> pPrint exe <+> text "with arguments" <+> pPrint args <+> text "stopped."
  pPrint (NoSuchPid exe args) =
    text "Could not find PID for process running " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text "."

instance HFErrorProject ProcessError where
  eBase _ = 840

  eNum NonZeroExit{}     = 841
  eNum (PTerminated _ _) = 842
  eNum (PStopped _ _)    = 843
  eNum (NoSuchPid _ _)   = 844

  eDesc _ = "A process exited prematurely."
