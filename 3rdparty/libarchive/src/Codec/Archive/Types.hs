module Codec.Archive.Types ( -- * Concrete (Haskell) data types
                             Entry (..)
                           , EntryContent (..)
                           , Ownership (..)
                           , ModTime
                           , Id
                           , Permissions
                           , ArchiveEncryption (..)
                           , ArchiveResult (..)
                           -- * Foreign types
                           , module Codec.Archive.Types.Foreign
                           -- * Callbacks
                           , ArchiveOpenCallback
                           , ArchiveCloseCallback
                           , ArchiveSwitchCallback
                           -- * Marshalling functions
                           , resultToErr
                           ) where

import           Codec.Archive.Types.Foreign
import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Foreign.C.Types             (CInt, CLong, CTime)
import           Foreign.Ptr                 (Ptr)
import           System.Posix.Types          (CMode (..))

type ArchiveOpenCallback a = Ptr Archive -> Ptr a -> IO ArchiveResult
type ArchiveCloseCallback a = Ptr Archive -> Ptr a -> IO ArchiveResult
type ArchiveSwitchCallback a b = Ptr Archive -> Ptr a -> Ptr b -> IO ArchiveResult

resultToErr :: ArchiveResult -> CInt
resultToErr = fromIntegral . fromEnum

data ArchiveEncryption = HasEncryption
                       | NoEncryption
                       | EncryptionUnsupported
                       | EncryptionUnknown
                       deriving (Eq)

-- TODO: support everything here: http://hackage.haskell.org/package/tar/docs/Codec-Archive-Tar-Entry.html#t:EntryContent
data EntryContent = NormalFile !BS.ByteString
                  | Directory
                  | Symlink !FilePath !Symlink
                  | Hardlink !FilePath
    deriving (Show, Eq, Ord)

data Entry = Entry { filepath    :: !FilePath
                   , content     :: EntryContent
                   , permissions :: !Permissions
                   , ownership   :: !Ownership
                   , time        :: !(Maybe ModTime)
                   }
    deriving (Show, Eq, Ord)

data Ownership = Ownership { userName  :: !(Maybe String)
                           , groupName :: !(Maybe String)
                           , ownerId   :: !Id
                           , groupId   :: !Id
                           }
    deriving (Eq, Show, Ord)

type Permissions = CMode
type ModTime = (CTime, CLong)

-- | A user or group ID
type Id = Int64
