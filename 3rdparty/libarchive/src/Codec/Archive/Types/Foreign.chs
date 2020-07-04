{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Codec.Archive.Types.Foreign ( -- * Callbacks
                                     ArchiveReadCallback
                                   , ArchiveSkipCallback
                                   , ArchiveSeekCallback
                                   , ArchiveWriteCallback
                                   , ArchiveCloseCallbackRaw
                                   , ArchiveOpenCallbackRaw
                                   , ArchiveSwitchCallbackRaw
                                   , ArchivePassphraseCallback
                                   -- * Abstract types
                                   , Archive
                                   , ArchiveEntry
                                   , Stat
                                   , LinkResolver
                                   -- * Enum types
                                   , ArchiveResult (..)
                                   , FileType (..)
                                   , Symlink (..)
                                   -- * Macros
                                   , Flags (..)
                                   , ArchiveFilter (..)
                                   , ArchiveFormat (..)
                                   , ArchiveCapabilities (..)
                                   , ReadDiskFlags (..)
                                   , TimeFlag (..)
                                   , EntryACL (..)
                                   -- * libarchive types
                                   , LaInt64
                                   , LaSSize
                                   ) where

import           Control.DeepSeq    (NFData)
import           Control.Exception  (Exception)
import           Data.Bits          (Bits (..))
import           Foreign.C.String   (CString)
import           Foreign.C.Types    (CInt, CSize)
import           Foreign.Ptr        (Ptr)
import           GHC.Generics       (Generic)

#include <archive.h>
#include <archive_entry.h>

type LaInt64 = {# type la_int64_t #}
type LaSSize = {# type la_ssize_t #}

{# enum define ArchiveResult { ARCHIVE_OK as ArchiveOk
                             , ARCHIVE_EOF as ArchiveEOF
                             , ARCHIVE_RETRY as ArchiveRetry
                             , ARCHIVE_WARN as ArchiveWarn
                             , ARCHIVE_FAILED as ArchiveFailed
                             , ARCHIVE_FATAL as ArchiveFatal
                             } deriving (Eq, Show, Generic, NFData, Exception)
  #}

{# enum define FileType { AE_IFREG as FtRegular
                        , AE_IFLNK as FtLink
                        , AE_IFSOCK as FtSocket
                        , AE_IFCHR as FtCharacter
                        , AE_IFBLK as FtBlock
                        , AE_IFDIR as FtDirectory
                        , AE_IFIFO as FtFifo
                        } deriving (Eq, Ord)
  #}

{# enum define Symlink { AE_SYMLINK_TYPE_UNDEFINED as SymlinkUndefined
                       , AE_SYMLINK_TYPE_FILE as SymlinkFile
                       , AE_SYMLINK_TYPE_DIRECTORY as SymlinkDirectory
                       } deriving (Show, Eq, Ord)
  #}

{# enum define ArchiveFilter { ARCHIVE_FILTER_NONE as ArchiveFilterNone
                             , ARCHIVE_FILTER_GZIP as ArchiveFilterGzip
                             , ARCHIVE_FILTER_BZIP2 as ArchiveFilterBzip2
                             , ARCHIVE_FILTER_COMPRESS as ArchiveFilterCompress
                             , ARCHIVE_FILTER_PROGRAM as ArchiveFilterProgram
                             , ARCHIVE_FILTER_LZMA as ArchiveFilterLzma
                             , ARCHIVE_FILTER_XZ as ArchiveFilterXz
                             , ARCHIVE_FILTER_UU as ArchiveFilterUu
                             , ARCHIVE_FILTER_RPM as ArchiveFilterRpm
                             , ARCHIVE_FILTER_LZIP as ArchiveFilterLzip
                             , ARCHIVE_FILTER_LRZIP as ArchiveFilterLrzip
                             , ARCHIVE_FILTER_LZOP as ArchiveFilterLzop
                             , ARCHIVE_FILTER_GRZIP as ArchiveFilterGrzip
                             , ARCHIVE_FILTER_LZ4 as ArchiveFilterLz4
                             , ARCHIVE_FILTER_ZSTD as ArchiveFilterZstd
                             }
  #}

{# enum define ArchiveFormat { ARCHIVE_FORMAT_CPIO as ArchiveFormatCpio
                             , ARCHIVE_FORMAT_CPIO_POSIX as ArchiveFormatCpioPosix
                             , ARCHIVE_FORMAT_CPIO_BIN_LE as ArchiveFormatCpioBinLe
                             , ARCHIVE_FORMAT_CPIO_BIN_BE as ArchiveFormatCpioBinBe
                             , ARCHIVE_FORMAT_CPIO_SVR4_NOCRC as ArchiveFormatCpioSvr4Nocrc
                             , ARCHIVE_FORMAT_CPIO_SVR4_CRC as ArchiveFormatCpioSvr4Crc
                             , ARCHIVE_FORMAT_CPIO_AFIO_LARGE as ArchiveFormatCpioAfioLarge
                             , ARCHIVE_FORMAT_SHAR as ArchiveFormatShar
                             , ARCHIVE_FORMAT_SHAR_BASE as ArchiveFormatSharBase
                             , ARCHIVE_FORMAT_SHAR_DUMP as ArchiveFormatSharDump
                             , ARCHIVE_FORMAT_TAR as ArchiveFormatTar
                             , ARCHIVE_FORMAT_TAR_USTAR as ArchiveFormatTarUstar
                             , ARCHIVE_FORMAT_TAR_PAX_INTERCHANGE as ArchiveFormatTarPaxInterchange
                             , ARCHIVE_FORMAT_TAR_PAX_RESTRICTED as ArchiveFormatTarPaxRestricted
                             , ARCHIVE_FORMAT_TAR_GNUTAR as ArchiveFormatTarGnutar
                             , ARCHIVE_FORMAT_ISO9660 as ArchiveFormatIso9660
                             , ARCHIVE_FORMAT_ISO9660_ROCKRIDGE as ArchiveFormatIso9660Rockridge
                             , ARCHIVE_FORMAT_ZIP as ArchiveFormatZip
                             , ARCHIVE_FORMAT_EMPTY as ArchiveFormatEmpty
                             , ARCHIVE_FORMAT_AR as ArchiveFormatAr
                             , ARCHIVE_FORMAT_AR_GNU as ArchiveFormatArGnu
                             , ARCHIVE_FORMAT_AR_BSD as ArchiveFormatArBsd
                             , ARCHIVE_FORMAT_MTREE as ArchiveFormatMtree
                             , ARCHIVE_FORMAT_RAW as ArchiveFormatRaw
                             , ARCHIVE_FORMAT_XAR as ArchiveFormatXar
                             , ARCHIVE_FORMAT_LHA as ArchiveFormatLha
                             , ARCHIVE_FORMAT_CAB as ArchiveFormatCab
                             , ARCHIVE_FORMAT_RAR as ArchiveFormatRar
                             , ARCHIVE_FORMAT_7ZIP as ArchiveFormat7zip
                             , ARCHIVE_FORMAT_WARC as ArchiveFormatWarc
                             , ARCHIVE_FORMAT_RAR_V5 as ArchiveFormatRarV5
                             } deriving (Eq)
  #}

-- | Abstract type
data Archive

-- | Abstract type
data ArchiveEntry

data Stat

data LinkResolver

type ArchiveReadCallback a b = Ptr Archive -> Ptr a -> Ptr (Ptr b) -> IO LaSSize
type ArchiveSkipCallback a = Ptr Archive -> Ptr a -> LaInt64 -> IO LaInt64
type ArchiveSeekCallback a = Ptr Archive -> Ptr a -> LaInt64 -> CInt -> IO LaInt64
type ArchiveWriteCallback a b = Ptr Archive -> Ptr a -> Ptr b -> CSize -> IO LaSSize
type ArchiveOpenCallbackRaw a = Ptr Archive -> Ptr a -> IO CInt
type ArchiveCloseCallbackRaw a = Ptr Archive -> Ptr a -> IO CInt
type ArchiveSwitchCallbackRaw a b = Ptr Archive -> Ptr a -> Ptr b -> IO CInt
type ArchivePassphraseCallback a = Ptr Archive -> Ptr a -> IO CString

newtype Flags = Flags CInt

newtype ReadDiskFlags = ReadDiskFlags CInt

newtype TimeFlag = TimeFlag CInt

newtype EntryACL = EntryACL CInt

newtype ArchiveCapabilities = ArchiveCapabilities CInt
    deriving (Eq)

instance Semigroup ArchiveCapabilities where
    (<>) (ArchiveCapabilities x) (ArchiveCapabilities y) = ArchiveCapabilities (x .|. y)

instance Monoid ArchiveCapabilities where
    mempty = ArchiveCapabilities 0
    mappend = (<>)

instance Semigroup ReadDiskFlags where
    (<>) (ReadDiskFlags x) (ReadDiskFlags y) = ReadDiskFlags (x .|. y)

instance Semigroup Flags where
    (<>) (Flags x) (Flags y) = Flags (x .|. y)

instance Monoid Flags where
    mempty = Flags 0
    mappend = (<>)

instance Semigroup EntryACL where
    (<>) (EntryACL x) (EntryACL y) = EntryACL (x .|. y)

-- TODO: `has` function for EntryACL
