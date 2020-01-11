{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

module GHCup.Errors where

import           GHCup.Types

import           Control.Exception.Safe
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Data.Versions
import           Haskus.Utils.Variant
import           HPath



    ------------------------
    --[ Low-level errors ]--
    ------------------------



-- | A compatible platform could not be found.
data NoCompatiblePlatform = NoCompatiblePlatform String -- the platform we got
  deriving Show

-- | Unable to find a download for the requested versio/distro.
data NoDownload = NoDownload
  deriving Show

-- | The Architecture is unknown and unsupported.
data NoCompatibleArch = NoCompatibleArch String
  deriving Show

-- | Unable to figure out the distribution of the host.
data DistroNotFound = DistroNotFound
  deriving Show

-- | The archive format is unknown. We don't know how to extract it.
data UnknownArchive = UnknownArchive ByteString
  deriving Show

-- | The scheme is not supported (such as ftp).
data UnsupportedScheme = UnsupportedScheme
  deriving Show

-- | Unable to copy a file.
data CopyError = CopyError String
  deriving Show

-- | Unable to find a tag of a tool.
data TagNotFound = TagNotFound Tag Tool
  deriving Show

-- | The tool (such as GHC) is already installed with that version.
data AlreadyInstalled = AlreadyInstalled Tool Version
  deriving Show

-- | The tool is not installed. Some operations rely on a tool
-- to be installed (such as setting the current GHC version).
data NotInstalled = NotInstalled Tool Version
  deriving Show

-- | JSON decoding failed.
data JSONError = JSONDecodeError String
  deriving Show

-- | A file that is supposed to exist does not exist
-- (e.g. when we use file scheme to "download" something).
data FileDoesNotExistError = FileDoesNotExistError ByteString
  deriving Show

-- | File digest verification failed.
data DigestError = DigestError Text Text
  deriving Show

-- | Unexpected HTTP status.
data HTTPStatusError = HTTPStatusError Int
  deriving Show

-- | The 'Location' header was expected during a 3xx redirect, but not found.
data NoLocationHeader = NoLocationHeader
  deriving Show

-- | Too many redirects.
data TooManyRedirs = TooManyRedirs
  deriving Show



    -------------------------
    --[ High-level errors ]--
    -------------------------

-- | A download failed. The underlying error is encapsulated.
data DownloadFailed = forall es . Show (V es) => DownloadFailed (V es)

deriving instance Show DownloadFailed


-- | A build failed.
data BuildFailed = forall es . Show (V es) => BuildFailed (Path Abs) (V es)

deriving instance Show BuildFailed


-- | Setting the current GHC version failed.
data GHCupSetError = forall es . Show (V es) => GHCupSetError (V es)

deriving instance Show GHCupSetError


    ---------------------------------------------
    --[ True Exceptions (e.g. for MonadThrow) ]--
    ---------------------------------------------


-- | Parsing failed.
data ParseError = ParseError String
  deriving Show

instance Exception ParseError
