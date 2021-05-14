{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes           #-}
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

#if !defined(TAR)
import           Codec.Archive
#else
import qualified Codec.Archive.Tar             as Tar
#endif
import           Control.Exception.Safe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions
import           Haskus.Utils.Variant
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass
import           URI.ByteString



    ------------------------
    --[ Low-level errors ]--
    ------------------------



-- | A compatible platform could not be found.
data NoCompatiblePlatform = NoCompatiblePlatform String -- the platform we got
  deriving Show

instance Pretty NoCompatiblePlatform where
  pPrint (NoCompatiblePlatform str') =
    text ("Could not find a compatible platform. Got: " ++ str')

-- | Unable to find a download for the requested versio/distro.
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
    text [i|The archive format is unknown. We don't know how to extract the file "#{file}"|]

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

-- | Unable to find a tag of a tool.
data TagNotFound = TagNotFound Tag Tool
  deriving Show

instance Pretty TagNotFound where
  pPrint (TagNotFound tag tool) =
    text "Unable to find tag" <+> pPrint tag <+> text [i|of tool "#{tool}"|]

-- | Unable to find the next version of a tool (the one after the currently
-- set one).
data NextVerNotFound = NextVerNotFound Tool
  deriving Show

instance Pretty NextVerNotFound where
  pPrint (NextVerNotFound tool) =
    text [i|Unable to find next (the one after the currently set one) version of tool "#{tool}"|]

-- | The tool (such as GHC) is already installed with that version.
data AlreadyInstalled = AlreadyInstalled Tool Version
  deriving Show

instance Pretty AlreadyInstalled where
  pPrint (AlreadyInstalled tool ver') =
    text [i|#{tool}-#{prettyShow ver'} is already installed|]

-- | The tool is not installed. Some operations rely on a tool
-- to be installed (such as setting the current GHC version).
data NotInstalled = NotInstalled Tool GHCTargetVersion
  deriving Show

instance Pretty NotInstalled where
  pPrint (NotInstalled tool ver) =
    text [i|The version "#{prettyShow ver}" of the tool "#{tool}" is not installed.|]

-- | An executable was expected to be in PATH, but was not found.
data NotFoundInPATH = NotFoundInPATH FilePath
  deriving Show

instance Pretty NotFoundInPATH where
  pPrint (NotFoundInPATH exe) =
    text [i|The exe "#{exe}" was not found in PATH.|]

-- | JSON decoding failed.
data JSONError = JSONDecodeError String
  deriving Show

instance Pretty JSONError where
  pPrint (JSONDecodeError err) =
    text [i|JSON decoding failed with: #{err}|]

-- | A file that is supposed to exist does not exist
-- (e.g. when we use file scheme to "download" something).
data FileDoesNotExistError = FileDoesNotExistError FilePath
  deriving Show

instance Pretty FileDoesNotExistError where
  pPrint (FileDoesNotExistError file) =
    text [i|File "#{file}" does not exist.|]

data TarDirDoesNotExist = TarDirDoesNotExist TarDir
  deriving Show

instance Pretty TarDirDoesNotExist where
  pPrint (TarDirDoesNotExist dir) =
    text "Tar directory does not exist:" <+> pPrint dir

-- | File digest verification failed.
data DigestError = DigestError Text Text
  deriving Show

instance Pretty DigestError where
  pPrint (DigestError currentDigest expectedDigest) =
    text [i|Digest error: expected "#{expectedDigest}", but got "#{currentDigest}"|]

-- | Unexpected HTTP status.
data HTTPStatusError = HTTPStatusError Int
  deriving Show

instance Pretty HTTPStatusError where
  pPrint (HTTPStatusError status) =
    text [i|Unexpected HTTP status: #{status}|]

-- | The 'Location' header was expected during a 3xx redirect, but not found.
data NoLocationHeader = NoLocationHeader
  deriving Show

instance Pretty NoLocationHeader where
  pPrint NoLocationHeader =
    text [i|The 'Location' header was expected during a 3xx redirect, but not found.|]

-- | Too many redirects.
data TooManyRedirs = TooManyRedirs
  deriving Show

instance Pretty TooManyRedirs where
  pPrint TooManyRedirs =
    text [i|Too many redirections.|]

-- | A patch could not be applied.
data PatchFailed = PatchFailed
  deriving Show

instance Pretty PatchFailed where
  pPrint PatchFailed =
    text [i|A patch could not be applied.|]

-- | The tool requirements could not be found.
data NoToolRequirements = NoToolRequirements
  deriving Show

instance Pretty NoToolRequirements where
  pPrint NoToolRequirements =
    text [i|The Tool requirements could not be found.|]

data InvalidBuildConfig = InvalidBuildConfig Text
  deriving Show

instance Pretty InvalidBuildConfig where
  pPrint (InvalidBuildConfig reason) =
    text [i|The build config is invalid. Reason was: #{reason}|]

data NoToolVersionSet = NoToolVersionSet Tool
  deriving Show

instance Pretty NoToolVersionSet where
  pPrint (NoToolVersionSet tool) =
    text [i|No version is set for tool "#{tool}".|]


    -------------------------
    --[ High-level errors ]--
    -------------------------

-- | A download failed. The underlying error is encapsulated.
data DownloadFailed = forall x xs . (Show x, Show (V xs), Pretty x, Pretty (V xs)) => DownloadFailed (V (x ': xs))

instance Pretty DownloadFailed where
  pPrint (DownloadFailed reason) =
    text "Download failed:" <+> pPrint reason

deriving instance Show DownloadFailed


-- | A build failed.
data BuildFailed = forall es . Show (V es) => BuildFailed FilePath (V es)

instance Pretty BuildFailed where
  pPrint (BuildFailed path reason) =
    text [i|BuildFailed failed in dir "#{path}": #{reason}|]

deriving instance Show BuildFailed


-- | Setting the current GHC version failed.
data GHCupSetError = forall es . Show (V es) => GHCupSetError (V es)

instance Pretty GHCupSetError where
  pPrint (GHCupSetError reason) =
    text [i|Setting the current GHC version failed: #{reason}|]

deriving instance Show GHCupSetError


    ---------------------------------------------
    --[ True Exceptions (e.g. for MonadThrow) ]--
    ---------------------------------------------


-- | Parsing failed.
data ParseError = ParseError String
  deriving Show

instance Pretty ParseError where
  pPrint (ParseError reason) =
    text [i|Parsing failed: #{reason}|]

instance Exception ParseError


data UnexpectedListLength = UnexpectedListLength String
  deriving Show

instance Pretty UnexpectedListLength where
  pPrint (UnexpectedListLength reason) =
    text [i|List length unexpected: #{reason}|]

instance Exception UnexpectedListLength



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
    text [i|Failed to parse URI. Malformed scheme: #{reason}|]
  pPrint MalformedUserInfo =
    text [i|Failed to parse URI. Malformed user info.|]
  pPrint MalformedQuery =
    text [i|Failed to parse URI. Malformed query.|]
  pPrint MalformedFragment =
    text [i|Failed to parse URI. Malformed fragment.|]
  pPrint MalformedHost =
    text [i|Failed to parse URI. Malformed host.|]
  pPrint MalformedPort =
    text [i|Failed to parse URI. Malformed port.|]
  pPrint MalformedPath =
    text [i|Failed to parse URI. Malformed path.|]
  pPrint (OtherError err) =
    text [i|Failed to parse URI: #{err}|]

#if !defined(TAR)
instance Pretty ArchiveResult where
  pPrint ArchiveFatal = text "Archive result: fatal"
  pPrint ArchiveFailed = text "Archive result: failed"
  pPrint ArchiveWarn = text "Archive result: warning"
  pPrint ArchiveRetry = text "Archive result: retry"
  pPrint ArchiveOk = text "Archive result: Ok"
  pPrint ArchiveEOF = text "Archive result: EOF"
#else
instance Pretty Tar.FormatError where
  pPrint Tar.TruncatedArchive = text "Truncated archive"
  pPrint Tar.ShortTrailer = text "Short trailer"
  pPrint Tar.BadTrailer = text "Bad trailer"
  pPrint Tar.TrailingJunk = text "Trailing junk"
  pPrint Tar.ChecksumIncorrect = text "Checksum incorrect"
  pPrint Tar.NotTarFormat = text "Not a tar format"
  pPrint Tar.UnrecognisedTarFormat = text "Unrecognised tar format"
  pPrint Tar.HeaderBadNumericEncoding = text "Header has bad numeric encoding"
#endif
