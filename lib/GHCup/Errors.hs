{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances           #-}

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



    ------------------------
    --[ Low-level errors ]--
    ------------------------



-- | A compatible platform could not be found.
data NoCompatiblePlatform = NoCompatiblePlatform String -- the platform we got
  deriving Show

instance Pretty NoCompatiblePlatform where
  pPrint (NoCompatiblePlatform str') =
    text ("Could not find a compatible platform. Got: " ++ str')

-- | Unable to find a download for the requested version/distro.
data NoDownload = NoDownload
  deriving Show

instance Pretty NoDownload where
  pPrint NoDownload =
    text "Unable to find a download for the requested version/distro."

-- | No update available or necessary.
data NoUpdate = NoUpdate
  deriving Show

instance Pretty NoUpdate where
  pPrint NoUpdate = text "No update available or necessary."

-- | The Architecture is unknown and unsupported.
data NoCompatibleArch = NoCompatibleArch String
  deriving Show

instance Pretty NoCompatibleArch where
  pPrint (NoCompatibleArch arch) =
    text ("The Architecture is unknown or unsupported. Got: " ++ arch)

-- | Unable to figure out the distribution of the host.
data DistroNotFound = DistroNotFound
  deriving Show

instance Pretty DistroNotFound where
  pPrint DistroNotFound =
    text "Unable to figure out the distribution of the host."

-- | The archive format is unknown. We don't know how to extract it.
data UnknownArchive = UnknownArchive FilePath
  deriving Show

instance Pretty UnknownArchive where
  pPrint (UnknownArchive file) =
    text $ "The archive format is unknown. We don't know how to extract the file " <> file

-- | The scheme is not supported (such as ftp).
data UnsupportedScheme = UnsupportedScheme
  deriving Show

instance Pretty UnsupportedScheme where
  pPrint UnsupportedScheme = text "The scheme is not supported (such as ftp)."

-- | Unable to copy a file.
data CopyError = CopyError String
  deriving Show

instance Pretty CopyError where
  pPrint (CopyError reason) =
    text ("Unable to copy a file. Reason was: " ++ reason)

-- | Unable to merge file trees.
data MergeFileTreeError = MergeFileTreeError IOException FilePath FilePath
  deriving Show

instance Pretty MergeFileTreeError where
  pPrint (MergeFileTreeError e from to) =
    text "Failed to merge file tree from" <+> text from <+> text "to" <+> text to <+> text "\nexception was:" <+> text (displayException e)
     <+> text "\n...you may need to delete" <+> text to <+> text "manually. Make sure it's gone."

-- | Unable to find a tag of a tool.
data TagNotFound = TagNotFound Tag Tool
  deriving Show

instance Pretty TagNotFound where
  pPrint (TagNotFound tag tool) =
    text "Unable to find tag" <+> pPrint tag <+> text "of tool" <+> pPrint tool

-- | Unable to find the next version of a tool (the one after the currently
-- set one).
data NextVerNotFound = NextVerNotFound Tool
  deriving Show

instance Pretty NextVerNotFound where
  pPrint (NextVerNotFound tool) =
    text "Unable to find next (the one after the currently set one) version of tool" <+> pPrint tool

-- | The tool (such as GHC) is already installed with that version.
data AlreadyInstalled = AlreadyInstalled Tool Version
  deriving Show

instance Pretty AlreadyInstalled where
  pPrint (AlreadyInstalled tool ver') =
    (pPrint tool <> text "-" <> pPrint ver') <+> text "is already installed;"
    <+> text "if you really want to reinstall it, you may want to run 'ghcup install" <+> pPrint tool <+> text "--force" <+> (pPrint ver' <> text "'")


-- | The Directory is supposed to be empty, but wasn't.
data DirNotEmpty = DirNotEmpty {path :: FilePath}
  deriving Show

instance Pretty DirNotEmpty where
  pPrint (DirNotEmpty path) = do
    text $ "The directory was expected to be empty, but isn't: " <> path

-- | The tool is not installed. Some operations rely on a tool
-- to be installed (such as setting the current GHC version).
data NotInstalled = NotInstalled Tool GHCTargetVersion
  deriving Show

instance Pretty NotInstalled where
  pPrint (NotInstalled tool ver) =
    text "The version" <+> pPrint ver <+> text "of the tool" <+> pPrint tool <+> text "is not installed."

data UninstallFailed = UninstallFailed FilePath [FilePath]
  deriving Show

instance Pretty UninstallFailed where
  pPrint (UninstallFailed dir files) =
    text "The following files survived uninstallation: " <+> pPrint files <+> text "...consider removing" <+> pPrint dir <+> text "manually."

-- | An executable was expected to be in PATH, but was not found.
data NotFoundInPATH = NotFoundInPATH FilePath
  deriving Show

instance Exception NotFoundInPATH

instance Pretty NotFoundInPATH where
  pPrint (NotFoundInPATH exe) =
    text $ "The exe " <> exe <> " was not found in PATH."

-- | JSON decoding failed.
data JSONError = JSONDecodeError String
  deriving Show

instance Pretty JSONError where
  pPrint (JSONDecodeError err) =
    text $ "JSON decoding failed with: " <> err

-- | A file that is supposed to exist does not exist
-- (e.g. when we use file scheme to "download" something).
data FileDoesNotExistError = FileDoesNotExistError FilePath
  deriving Show

instance Pretty FileDoesNotExistError where
  pPrint (FileDoesNotExistError file) =
    text $ "File " <> file <> " does not exist."

-- | The file already exists
-- (e.g. when we use isolated installs with the same path).
-- (e.g. This is done to prevent any overwriting)
data FileAlreadyExistsError = FileAlreadyExistsError FilePath
  deriving Show

instance Pretty FileAlreadyExistsError where
  pPrint (FileAlreadyExistsError file) =
    text $ "File " <> file <> " Already exists."

data TarDirDoesNotExist = TarDirDoesNotExist TarDir
  deriving Show

instance Pretty TarDirDoesNotExist where
  pPrint (TarDirDoesNotExist dir) =
    text "Tar directory does not exist:" <+> pPrint dir

-- | File digest verification failed.
data DigestError = DigestError FilePath Text Text
  deriving Show

instance Pretty DigestError where
  pPrint (DigestError fp currentDigest expectedDigest) =
    text "Digest error for" <+> text (fp <> ": expected")
      <+> text (T.unpack expectedDigest) <+> text "but got" <+> pPrint currentDigest <+> text
      "\nConsider removing the file in case it's cached and try again."

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

-- | File digest verification failed.
data GPGError = forall xs . (ToVariantMaybe DownloadFailed xs, PopVariant DownloadFailed xs, Show (V xs), Pretty (V xs)) => GPGError (V xs)

deriving instance Show GPGError

instance Pretty GPGError where
  pPrint (GPGError reason) = text "GPG verify failed:" <+> pPrint reason

-- | Unexpected HTTP status.
data HTTPStatusError = HTTPStatusError Int (M.Map (CI ByteString) ByteString)
  deriving Show

instance Pretty HTTPStatusError where
  pPrint (HTTPStatusError status _) =
    text "Unexpected HTTP status:" <+> pPrint status

-- | Malformed headers.
data MalformedHeaders = MalformedHeaders Text
  deriving Show

instance Pretty MalformedHeaders where
  pPrint (MalformedHeaders h) =
    text "Headers are malformed: " <+> pPrint h

-- | Unexpected HTTP status.
data HTTPNotModified = HTTPNotModified Text
  deriving Show

instance Pretty HTTPNotModified where
  pPrint (HTTPNotModified etag) =
    text "Remote resource not modifed, etag was:" <+> pPrint etag

-- | The 'Location' header was expected during a 3xx redirect, but not found.
data NoLocationHeader = NoLocationHeader
  deriving Show

instance Pretty NoLocationHeader where
  pPrint NoLocationHeader =
    text "The 'Location' header was expected during a 3xx redirect, but not found."

-- | Too many redirects.
data TooManyRedirs = TooManyRedirs
  deriving Show

instance Pretty TooManyRedirs where
  pPrint TooManyRedirs =
    text "Too many redirections."

-- | A patch could not be applied.
data PatchFailed = PatchFailed
  deriving Show

instance Pretty PatchFailed where
  pPrint PatchFailed =
    text "A patch could not be applied."

-- | The tool requirements could not be found.
data NoToolRequirements = NoToolRequirements
  deriving Show

instance Pretty NoToolRequirements where
  pPrint NoToolRequirements =
    text "The Tool requirements could not be found."

data InvalidBuildConfig = InvalidBuildConfig Text
  deriving Show

instance Pretty InvalidBuildConfig where
  pPrint (InvalidBuildConfig reason) =
    text "The build config is invalid. Reason was:" <+> pPrint reason

data NoToolVersionSet = NoToolVersionSet Tool
  deriving Show

instance Pretty NoToolVersionSet where
  pPrint (NoToolVersionSet tool) =
    text "No version is set for tool" <+> pPrint tool <+> text "."

data NoNetwork = NoNetwork
  deriving Show

instance Pretty NoNetwork where
  pPrint NoNetwork =
    text "A download was required or requested, but '--offline' was specified."

data HadrianNotFound = HadrianNotFound
  deriving Show

instance Pretty HadrianNotFound where
  pPrint HadrianNotFound =
    text "Could not find Hadrian build files. Does this GHC version support Hadrian builds?"

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

    -------------------------
    --[ High-level errors ]--
    -------------------------

-- | A download failed. The underlying error is encapsulated.
data DownloadFailed = forall xs . (ToVariantMaybe DownloadFailed xs, PopVariant DownloadFailed xs, Show (V xs), Pretty (V xs)) => DownloadFailed (V xs)

instance Pretty DownloadFailed where
  pPrint (DownloadFailed reason) =
    case reason of
      VMaybe (_ :: DownloadFailed) -> pPrint reason
      _ -> text "Download failed:" <+> pPrint reason

deriving instance Show DownloadFailed

data InstallSetError = forall xs1 xs2 . (Show (V xs1), Pretty (V xs1), Show (V xs2), Pretty (V xs2)) => InstallSetError (V xs1) (V xs2)

instance Pretty InstallSetError where
  pPrint (InstallSetError reason1 reason2) =
     text "Both installation and setting the tool failed. Install error was:"
      <+> pPrint reason1
      <+> text "\nSet error was:"
      <+> pPrint reason2

deriving instance Show InstallSetError


-- | A build failed.
data BuildFailed = forall es . (ToVariantMaybe BuildFailed es, PopVariant BuildFailed es, Pretty (V es), Show (V es)) => BuildFailed FilePath (V es)

instance Pretty BuildFailed where
  pPrint (BuildFailed path reason) =
    case reason of
      VMaybe (_ :: BuildFailed) -> pPrint reason
      _ -> text "BuildFailed failed in dir" <+> text (path <> ":") <+> pPrint reason

deriving instance Show BuildFailed


-- | Setting the current GHC version failed.
data GHCupSetError = forall es . (ToVariantMaybe GHCupSetError es, PopVariant GHCupSetError es, Show (V es), Pretty (V es)) => GHCupSetError (V es)

instance Pretty GHCupSetError where
  pPrint (GHCupSetError reason) =
    case reason of
      VMaybe (_ :: GHCupSetError) -> pPrint reason
      _ -> text "Setting the current GHC version failed:" <+> pPrint reason

deriving instance Show GHCupSetError


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


data UnexpectedListLength = UnexpectedListLength String
  deriving Show

instance Pretty UnexpectedListLength where
  pPrint (UnexpectedListLength reason) =
    text "List length unexpected:" <+> pPrint reason

instance Exception UnexpectedListLength

data NoUrlBase = NoUrlBase Text
  deriving Show

instance Pretty NoUrlBase where
  pPrint (NoUrlBase url) =
    text "Couldn't get a base filename from url" <+> pPrint url

instance Exception NoUrlBase



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

instance Pretty ArchiveResult where
  pPrint ArchiveFatal = text "Archive result: fatal"
  pPrint ArchiveFailed = text "Archive result: failed"
  pPrint ArchiveWarn = text "Archive result: warning"
  pPrint ArchiveRetry = text "Archive result: retry"
  pPrint ArchiveOk = text "Archive result: Ok"
  pPrint ArchiveEOF = text "Archive result: EOF"

instance Pretty T.Text where
  pPrint = text . T.unpack
