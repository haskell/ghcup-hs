-- | This module corresponds to @archive.h@
--
-- Functions in this module are stateful and hence take place in the 'IO'
-- monad.
module Codec.Archive.Foreign.Archive ( archiveReadHasEncryptedEntries
                                     -- * Version information
                                     , archiveVersionNumber
                                     , archiveVersionString
                                     , archiveVersionDetails
                                     , archiveZlibVersion
                                     , archiveLiblzmaVersion
                                     , archiveBzlibVersion
                                     , archiveLiblz4Version
                                     , archiveLibzstdVersion
                                     -- * Miscellany
                                     , archiveErrorString
                                     , archiveFormatName
                                     , archiveFormat
                                     , archiveClearError
                                     , archiveSetError
                                     , archiveCopyError
                                     , archiveFileCount
                                     , archiveFilterCount
                                     , archiveFilterBytes
                                     , archiveFilterCode
                                     , archiveFilterName
                                     -- * Read
                                     , archiveReadData
                                     , archiveReadNew
                                     , archiveReadSetOpenCallback
                                     , archiveReadSetSeekCallback
                                     , archiveReadSetSkipCallback
                                     , archiveReadSetSwitchCallback
                                     , archiveReadSetCallbackData2
                                     , archiveReadAddCallbackData
                                     , archiveReadAppendCallbackData
                                     , archiveReadPrependCallbackData
                                     , archiveReadSetReadCallback
                                     , archiveReadSetCloseCallback
                                     , archiveReadSetCallbackData
                                     , archiveReadSetFormat
                                     , archiveReadOpen
                                     , archiveReadOpenFilename
                                     , archiveReadOpenFilenameW
                                     , archiveReadOpenFilenames
                                     , archiveReadOpenMemory
                                     , archiveReadOpen1
                                     , archiveReadOpen2
                                     , archiveReadOpenFd
                                     , archiveReadOpenFILE
                                     , archiveReadNextHeader
                                     , archiveReadNextHeader2
                                     , archiveReadHeaderPosition
                                     , archiveReadFormatCapabilities
                                     , archiveSeekData
                                     , archiveReadDataBlock
                                     , archiveReadDataIntoFd
                                     , archiveReadSetFormatOption
                                     , archiveReadSetFilterOption
                                     , archiveReadSetOption
                                     , archiveReadSetOptions
                                     , archiveReadAddPassphrase
                                     , archiveReadSetPassphraseCallback
                                     , archiveReadExtract
                                     , archiveReadExtract2
                                     , archiveReadExtractSetProgressCallback
                                     , archiveReadExtractSetSkipFile
                                     , archiveReadClose
                                     , archiveReadFree
                                     , archiveReadSupportFilterAll
                                     , archiveReadSupportFilterBzip2
                                     , archiveReadSupportFilterCompress
                                     , archiveReadSupportFilterGzip
                                     , archiveReadSupportFilterGrzip
                                     , archiveReadSupportFilterLrzip
                                     , archiveReadSupportFilterLz4
                                     , archiveReadSupportFilterLzip
                                     , archiveReadSupportFilterLzma
                                     , archiveReadSupportFilterLzop
                                     , archiveReadSupportFilterNone
                                     , archiveReadSupportFilterProgram
                                     , archiveReadSupportFilterProgramSignature
                                     , archiveReadSupportFilterRpm
                                     , archiveReadSupportFilterUu
                                     , archiveReadSupportFilterXz
                                     , archiveReadSupportFormatAll
                                     , archiveReadSupportFormat7zip
                                     , archiveReadSupportFormatAr
                                     , archiveReadSupportFormatByCode
                                     , archiveReadSupportFormatCab
                                     , archiveReadSupportFormatCpio
                                     , archiveReadSupportFormatEmpty
                                     , archiveReadSupportFormatGnutar
                                     , archiveReadSupportFormatIso9660
                                     , archiveReadSupportFormatLha
                                     , archiveReadSupportFormatMtree
                                     , archiveReadSupportFormatRar
                                     , archiveReadSupportFormatRar5
                                     , archiveReadSupportFormatRaw
                                     , archiveReadSupportFormatTar
                                     , archiveReadSupportFormatWarc
                                     , archiveReadSupportFormatXar
                                     , archiveReadSupportFormatZip
                                     , archiveReadSupportFormatZipStreamable
                                     , archiveReadSupportFormatZipSeekable
                                     , archiveReadAppendFilter
                                     , archiveReadAppendFilterProgram
                                     , archiveReadAppendFilterProgramSignature
                                     -- * Write
                                     , archiveWriteOpenMemory
                                     , archiveWriteNew
                                     , archiveWriteData
                                     , archiveWriteOpen
                                     , archiveWriteClose
                                     , archiveWriteHeader
                                     , archiveWriteSetBytesPerBlock
                                     , archiveWriteGetBytesPerBlock
                                     , archiveWriteSetBytesInLastBlock
                                     , archiveWriteGetBytesInLastBlock
                                     , archiveWriteSetSkipFile
                                     , archiveWriteAddFilter
                                     , archiveWriteAddFilterByName
                                     , archiveWriteAddFilterB64encode
                                     , archiveWriteAddFilterBzip2
                                     , archiveWriteAddFilterCompress
                                     , archiveWriteAddFilterGrzip
                                     , archiveWriteAddFilterLrzip
                                     , archiveWriteAddFilterLz4
                                     , archiveWriteAddFilterLzma
                                     , archiveWriteAddFilterLzip
                                     , archiveWriteAddFilterLzop
                                     , archiveWriteAddFilterNone
                                     , archiveWriteAddFilterProgram
                                     , archiveWriteAddFilterUuencode
                                     , archiveWriteAddFilterXz
                                     , archiveWriteAddFilterZstd
                                     , archiveWriteSetFormat
                                     , archiveWriteSetFormatByName
                                     , archiveWriteSetFormatArBsd
                                     , archiveWriteSetFormatArSvr4
                                     , archiveWriteSetFormatCpio
                                     , archiveWriteSetFormatCpioNewc
                                     , archiveWriteSetFormatGnutar
                                     , archiveWriteSetFormatMtree
                                     , archiveWriteSetFormatMtreeClassic
                                     , archiveWriteSetFormatPax
                                     , archiveWriteSetFormatPaxRestricted
                                     , archiveWriteSetFormatZip
                                     , archiveWriteSetFormat7zip
                                     , archiveWriteSetFormatRaw
                                     , archiveWriteSetFormatShar
                                     , archiveWriteSetFormatSharDump
                                     , archiveWriteSetFormatUstar
                                     , archiveWriteSetFormatV7tar
                                     , archiveWriteSetFormatWarc
                                     , archiveWriteSetFormatXar
                                     , archiveWriteSetFormatFilterByExt
                                     , archiveWriteSetFormatFilterByExtDef
                                     , archiveWriteZipSetCompressionDeflate
                                     , archiveWriteZipSetCompressionStore
                                     , archiveWriteOpenFd
                                     , archiveWriteOpenFilenameW
                                     , archiveWriteOpenFilename
                                     , archiveWriteOpenFILE
                                     , archiveWriteDataBlock
                                     , archiveWriteFinishEntry
                                     , archiveWriteFail
                                     , archiveWriteFree
                                     , archiveWriteSetFormatOption
                                     , archiveWriteSetFilterOption
                                     , archiveWriteSetOption
                                     , archiveWriteSetOptions
                                     , archiveWriteSetPassphrase
                                     , archiveWriteSetPassphraseCallback
                                     -- * Write disk
                                     , archiveWriteDiskSetOptions
                                     , archiveWriteDiskNew
                                     , archiveWriteDiskSetSkipFile
                                     , archiveWriteDiskSetStandardLookup
                                     , archiveWriteDiskSetGroupLookup
                                     , archiveWriteDiskSetUserLookup
                                     , archiveWriteDiskGid
                                     , archiveWriteDiskUid
                                     -- * Read disk
                                     , archiveReadDiskNew
                                     , archiveReadDiskSetSymlinkLogical
                                     , archiveReadDiskSetSymlinkPhysical
                                     , archiveReadDiskSetSymlinkHybrid
                                     , archiveReadDiskEntryFromFile
                                     , archiveReadDiskGname
                                     , archiveReadDiskUname
                                     , archiveReadDiskSetStandardLookup
                                     , archiveReadDiskSetGnameLookup
                                     , archiveReadDiskSetUnameLookup
                                     , archiveReadDiskOpen
                                     , archiveReadDiskOpenW
                                     , archiveReadDiskDescend
                                     , archiveReadDiskCanDescend
                                     , archiveReadDiskCurrentFilesystem
                                     , archiveReadDiskCurrentFilesystemIsSynthetic
                                     , archiveReadDiskCurrentFilesystemIsRemote
                                     , archiveReadDiskSetAtimeRestored
                                     , archiveReadDiskSetBehavior
                                     , archiveReadDiskSetMatching
                                     , archiveReadDiskSetMetadataFilterCallback
                                     , archiveReadDiskNoAcl
                                     , archiveReadDiskNoFFlags
                                     -- * Version macros
                                     , archiveVersionNumberMacro
                                     , archiveVersionOnlyString
                                     , archiveVersionStringMacro
                                     -- * Capability macros
                                     , archiveReadFormatCapsNone
                                     , archiveReadFormatCapsEncryptData
                                     , archiveReadFormatCapsEncryptMetadata
                                     -- * Time matching macros
                                     , archiveMatchMTime
                                     , archiveMatchCTime
                                     , archiveMatchNewer
                                     , archiveMatchOlder
                                     , archiveMatchEqual
                                     -- * Entry flags
                                     , archiveExtractOwner
                                     , archiveExtractPerm
                                     , archiveExtractTime
                                     , archiveExtractNoOverwrite
                                     , archiveExtractUnlink
                                     , archiveExtractACL
                                     , archiveExtractFFlags
                                     , archiveExtractXattr
                                     , archiveExtractSecureSymlinks
                                     , archiveExtractSecureNoDotDot
                                     , archiveExtractNoAutodir
                                     , archiveExtractSparse
                                     , archiveExtractMacMetadata
                                     , archiveExtractNoHfsCompression
                                     , archiveExtractHfsCompressionForced
                                     , archiveExtractSecureNoAbsolutePaths
                                     , archiveExtractClearNoChangeFFlags
                                     , archiveExtractNoOverwriteNewer
                                     -- * Read disk flags
                                     , archiveReadDiskRestoreATime
                                     , archiveReadDiskHonorNoDump
                                     , archiveReadDiskMacCopyFile
                                     , archiveReadDiskNoTraverseMounts
                                     , archiveReadDiskNoXattr
                                     , archiveFree
                                     , archiveMatchExcluded
                                     , archiveMatchPathExcluded
                                     , archiveMatchSetInclusionRecursion
                                     , archiveMatchExcludePattern
                                     , archiveMatchExcludePatternW
                                     , archiveMatchIncludePattern
                                     , archiveMatchIncludePatternW
                                     , archiveMatchExcludePatternFromFile
                                     , archiveMatchExcludePatternFromFileW
                                     , archiveMatchIncludePatternFromFile
                                     , archiveMatchIncludePatternFromFileW
                                     , archiveMatchPathUnmatchedInclusions
                                     , archiveMatchPathUnmatchedInclusionsNext
                                     , archiveMatchPathUnmatchedInclusionsNextW
                                     , archiveMatchIncludeTime
                                     , archiveMatchIncludeDate
                                     , archiveMatchIncludeDateW
                                     , archiveMatchIncludeFileTime
                                     , archiveMatchIncludeFileTimeW
                                     , archiveMatchTimeExcluded
                                     , archiveMatchOwnerExcluded
                                     , archiveMatchExcludeEntry
                                     , archiveReadDataSkip
                                     , archiveMatchIncludeGname
                                     , archiveMatchIncludeGnameW
                                     , archiveMatchIncludeUname
                                     , archiveMatchIncludeUnameW
                                     , archiveMatchIncludeUid
                                     , archiveMatchIncludeGid
                                     , archiveErrno
                                     -- * Abstract types
                                     , Archive
                                     -- * Haskell types
                                     , ArchiveEncryption (..)
                                     -- * C types
                                     , FilePtr (..)
                                     -- * Enum types
                                     , ArchiveFilter (..)
                                     , ArchiveFormat (..)
                                     -- * Lower-level API types
                                     , Flags
                                     , ArchiveCapabilities
                                     , ReadDiskFlags
                                     , TimeFlag
                                     -- * Callback types
                                     , ArchiveReadCallback
                                     , ArchiveSkipCallback
                                     , ArchiveSeekCallback
                                     , ArchiveWriteCallback
                                     , ArchiveOpenCallback
                                     , ArchiveCloseCallback
                                     , ArchiveSwitchCallback
                                     , ArchiveOpenCallbackRaw
                                     , ArchiveCloseCallbackRaw
                                     , ArchiveSwitchCallbackRaw
                                     , ArchivePassphraseCallback
                                     -- * Callback constructors
                                     , noOpenCallback
                                     , mkReadCallback
                                     , mkSkipCallback
                                     , mkSeekCallback
                                     , mkWriteCallback
                                     , mkPassphraseCallback
                                     , mkOpenCallback
                                     , mkCloseCallback
                                     , mkSwitchCallback
                                     , mkWriteLookup
                                     , mkReadLookup
                                     , mkCleanup
                                     , mkMatch
                                     , mkFilter
                                     , mkExcludedCallback
                                     -- * Type synonyms
                                     , ArchiveEntryPtr
                                     , ArchivePtr
                                     , StatPtr
                                     -- * libarchive types
                                     , LaInt64
                                     , LaSSize
                                     ) where

{# import qualified Codec.Archive.Types.Foreign #}

import Codec.Archive.Foreign.Archive.Macros
import Codec.Archive.Types
import Control.Composition ((.*), (.**))
import Data.Coerce (coerce)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable (Storable (peek))
import System.Posix.Types (Fd (..))

-- destructors: use "dynamic" instead of "wrapper" (but we don't want that)
-- callbacks
foreign import ccall "wrapper" mkReadCallback :: ArchiveReadCallback a b -> IO (FunPtr (ArchiveReadCallback a b))
foreign import ccall "wrapper" mkSkipCallback :: ArchiveSkipCallback a -> IO (FunPtr (ArchiveSkipCallback a))
foreign import ccall "wrapper" mkSeekCallback :: ArchiveSeekCallback a -> IO (FunPtr (ArchiveSeekCallback a))
foreign import ccall "wrapper" mkWriteCallback :: ArchiveWriteCallback a b -> IO (FunPtr (ArchiveWriteCallback a b))
foreign import ccall "wrapper" mkOpenCallbackRaw :: ArchiveOpenCallbackRaw a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
foreign import ccall "wrapper" mkCloseCallbackRaw :: ArchiveCloseCallbackRaw a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
foreign import ccall "wrapper" mkSwitchCallbackRaw :: ArchiveSwitchCallbackRaw a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
foreign import ccall "wrapper" mkPassphraseCallback :: ArchivePassphraseCallback a -> IO (FunPtr (ArchivePassphraseCallback a))
foreign import ccall "wrapper" mkExcludedCallback :: (ArchivePtr -> Ptr a -> ArchiveEntryPtr -> IO ()) -> IO (FunPtr (ArchivePtr -> Ptr a -> ArchiveEntryPtr -> IO ()))

-- | Don't use an open callback. This is the recommended argument to 'archiveReadOpen'
noOpenCallback :: FunPtr (ArchiveOpenCallbackRaw a)
noOpenCallback = castPtrToFunPtr nullPtr

foreign import ccall "wrapper" mkWriteLookup :: (Ptr a -> CString -> LaInt64 -> IO LaInt64) -> IO (FunPtr (Ptr a -> CString -> LaInt64 -> IO LaInt64))
-- | Also for 'archiveReadDiskSetGnameLookup' and 'archiveReadDiskSetUnameLookup'
foreign import ccall "wrapper" mkReadLookup :: (Ptr a -> LaInt64 -> IO CString) -> IO (FunPtr (Ptr a -> LaInt64 -> IO CString))

-- | Can also be used with 'archiveReadExtractSetProgressCallback'
foreign import ccall "wrapper" mkCleanup :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "wrapper" mkMatch :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO ()))
foreign import ccall "wrapper" preMkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))

mkOpenCallback :: ArchiveOpenCallback a -> IO (FunPtr (ArchiveOpenCallbackRaw a))
mkOpenCallback f = let f' = fmap resultToErr .* f in mkOpenCallbackRaw f'

mkCloseCallback :: ArchiveCloseCallback a -> IO (FunPtr (ArchiveCloseCallbackRaw a))
mkCloseCallback f = let f' = fmap resultToErr .* f in mkCloseCallbackRaw f'

mkSwitchCallback :: ArchiveSwitchCallback a b -> IO (FunPtr (ArchiveSwitchCallbackRaw a b))
mkSwitchCallback f = let f' = fmap resultToErr .** f in mkSwitchCallbackRaw f'

mkFilter :: (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO Bool) -> IO (FunPtr (Ptr Archive -> Ptr a -> Ptr ArchiveEntry -> IO CInt))
mkFilter f = let f' = fmap boolToInt .** f in preMkFilter f'
    where boolToInt False = 0
          boolToInt True  = 1

#include <archive.h>

{#pointer *archive as ArchivePtr -> Archive #}
{#pointer *archive_entry as ArchiveEntryPtr -> ArchiveEntry #}
{#pointer *stat as StatPtr -> Stat #}
-- | @FILE*@ in C
{#pointer *FILE as FilePtr newtype#}

{#typedef size_t CSize#}
{#typedef wchar_t CWchar#}
{#typedef la_ssize_t LaSSize#}
{#typedef la_int64_t LaInt64#}
{#typedef time_t CTime#}

{#default in `CWString' [wchar_t*] castPtr#}

{# fun archive_zlib_version as ^ {} -> `CString' #}
{# fun archive_liblzma_version as ^ {} -> `CString' #}
{# fun archive_bzlib_version as ^ {} -> `CString' #}
{# fun archive_liblz4_version as ^ {} -> `CString' #}
{# fun archive_libzstd_version as ^ {} -> `CString' #}

{# fun archive_error_string as ^ { `ArchivePtr' } -> `CString' #}
{# fun archive_format_name as ^ { `ArchivePtr' } -> `CString' #}
{# fun archive_format as ^ { `ArchivePtr' } -> `ArchiveFormat' #}
{# fun archive_clear_error as ^ { `ArchivePtr' } -> `()' #}
{# fun archive_set_error as ^ { `ArchivePtr', `CInt', `CString' } -> `()' #}
{# fun archive_copy_error as ^ { `ArchivePtr', `ArchivePtr' } -> `()' #}
{# fun archive_file_count as ^ { `ArchivePtr' } -> `CInt' #}
{# fun pure archive_version_number as ^ {} -> `CInt' #}
{# fun pure archive_version_string as ^ {} -> `String' #}
{# fun pure archive_version_details as ^ {} -> `String' #}
{# fun archive_filter_count as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_filter_bytes as ^ { `ArchivePtr', `CInt' } -> `LaInt64' #}
{# fun archive_filter_code as ^ { `ArchivePtr', `CInt' } -> `Int' #}
{# fun archive_filter_name as ^ { `ArchivePtr', `CInt' } -> `CString' #}

{# fun archive_read_new as ^ {} -> `ArchivePtr' #}

{# fun archive_match_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_match_path_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_match_exclude_pattern as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_exclude_pattern_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_set_inclusion_recursion as ^ { `ArchivePtr', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_exclude_pattern_from_file as ^ { `ArchivePtr', `CString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_exclude_pattern_from_file_w as ^ { `ArchivePtr', `CWString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern_from_file as ^ { `ArchivePtr', `CString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_include_pattern_from_file_w as ^ { `ArchivePtr', `CWString', `Bool' } -> `ArchiveResult' #}
{# fun archive_match_path_unmatched_inclusions as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_match_path_unmatched_inclusions_next as ^ { `ArchivePtr', alloca- `CString' peek* } -> `ArchiveResult' #}
{# fun archive_match_path_unmatched_inclusions_next_w as ^ { `ArchivePtr', alloca- `CWString' peek* } -> `ArchiveResult' #}
{# fun archive_match_time_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}
{# fun archive_match_include_time as ^ { `ArchivePtr', coerce `TimeFlag', `CTime', `CLong' } -> `ArchiveResult' #}
{# fun archive_match_include_date as ^ { `ArchivePtr', coerce `TimeFlag', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_date_w as ^ { `ArchivePtr', coerce `TimeFlag', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_file_time as ^ { `ArchivePtr', coerce `TimeFlag', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_file_time_w as ^ { `ArchivePtr', coerce `TimeFlag', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_exclude_entry as ^ { `ArchivePtr', coerce `TimeFlag', `ArchiveEntryPtr' } -> `ArchiveResult' #}

{# fun archive_match_owner_excluded as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `Bool' #}

{# fun archive_read_set_open_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveOpenCallbackRaw a)' } -> `ArchiveResult' #}
{# fun archive_read_set_read_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveReadCallback a b)' } -> `ArchiveResult' #}
{# fun archive_read_set_seek_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveSeekCallback a)' } -> `ArchiveResult' #}
{# fun archive_read_set_skip_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveSeekCallback a)' } -> `ArchiveResult' #}
{# fun archive_read_set_close_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveCloseCallbackRaw a)' } -> `ArchiveResult' #}
{# fun archive_read_set_switch_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (ArchiveSwitchCallbackRaw a n)' } -> `ArchiveResult' #}

{# fun archive_read_set_callback_data as ^ { `ArchivePtr', castPtr `Ptr a' } -> `ArchiveResult' #}
{# fun archive_read_set_callback_data2 as ^ { `ArchivePtr', castPtr `Ptr a', `CUInt' } -> `ArchiveResult' #}
{# fun archive_read_add_callback_data as ^ { `ArchivePtr', castPtr `Ptr a', `CUInt' } -> `ArchiveResult' #}
{# fun archive_read_append_callback_data as ^ { `ArchivePtr', castPtr `Ptr a' } -> `ArchiveResult' #}
{# fun archive_read_prepend_callback_data as ^ { `ArchivePtr', castPtr `Ptr a' } -> `ArchiveResult' #}

{# fun archive_read_open1 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_open as ^ { `ArchivePtr'
                              , castPtr `Ptr a'
                              , castFunPtr `FunPtr (ArchiveOpenCallbackRaw a)'
                              , castFunPtr `FunPtr (ArchiveReadCallback a b)'
                              , castFunPtr `FunPtr (ArchiveCloseCallbackRaw a)'
                              } -> `ArchiveResult' #}
{# fun archive_read_open2 as ^ { `ArchivePtr'
                               , castPtr `Ptr a'
                               , castFunPtr `FunPtr (ArchiveOpenCallbackRaw a)'
                               , castFunPtr `FunPtr (ArchiveReadCallback a b)'
                               , castFunPtr `FunPtr (ArchiveSkipCallback a)'
                               , castFunPtr `FunPtr (ArchiveCloseCallbackRaw a)'
                               } -> `ArchiveResult' #}

{# fun archive_read_open_filename as ^ { `ArchivePtr', `CString', `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_filename_w as ^ { `ArchivePtr', `CWString', `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_filenames as ^ { `ArchivePtr', id `Ptr CString', `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_memory as ^ { `ArchivePtr', castPtr `Ptr a', `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_fd as ^ { `ArchivePtr', coerce `Fd', `CSize' } -> `ArchiveResult' #}
{# fun archive_read_open_FILE as ^ { `ArchivePtr', `FilePtr' } -> `ArchiveResult' #}
{# fun archive_read_next_header as ^ { `ArchivePtr', alloca- `ArchiveEntryPtr' peek* } -> `ArchiveResult' #}
{# fun archive_read_next_header2 as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `ArchiveResult' #}
{# fun archive_read_header_position as ^ { `ArchivePtr' } -> `LaInt64' #}
{# fun archive_read_has_encrypted_entries as ^ { `ArchivePtr' } -> `ArchiveEncryption' encryptionResult #}
{# fun archive_read_format_capabilities as ^ { `ArchivePtr' } -> `ArchiveCapabilities' ArchiveCapabilities #}
{# fun archive_read_data as ^ { `ArchivePtr', castPtr `Ptr a', `CSize' } -> `LaSSize' #}
{# fun archive_seek_data as ^ { `ArchivePtr', `LaInt64', `CInt' } -> `LaInt64' #}
{# fun archive_read_data_block as ^ { `ArchivePtr', castPtr `Ptr (Ptr a)', id `Ptr CSize', id `Ptr LaInt64' } -> `ArchiveResult' #}
{# fun archive_read_data_skip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_data_into_fd as ^ { `ArchivePtr', coerce `Fd' } -> `ArchiveResult' #}
{# fun archive_read_set_format_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_read_set_filter_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_read_set_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_read_set_options as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}

{# fun archive_read_add_passphrase as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_read_set_passphrase_callback as ^ { `ArchivePtr', castPtr `Ptr a', castFunPtr `FunPtr (ArchivePassphraseCallback a)' } -> `ArchiveResult' #}

{# fun archive_read_extract as ^ { `ArchivePtr', `ArchiveEntryPtr', coerce `Flags' } -> `ArchiveResult' #}
{# fun archive_read_extract2 as ^ { `ArchivePtr', `ArchiveEntryPtr', `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_extract_set_progress_callback as ^ { `ArchivePtr', castFunPtr `FunPtr (Ptr a -> IO ())', castPtr `Ptr a' } -> `()' id #}
{# fun archive_read_extract_set_skip_file as ^ { `ArchivePtr', `LaInt64', `LaInt64' } -> `()' #}
{# fun archive_read_close as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_free as ^ { `ArchivePtr' } -> `ArchiveResult' #}

{# fun archive_write_new as ^ {} -> `ArchivePtr' #}
{# fun archive_write_set_bytes_per_block as ^ { `ArchivePtr', `CInt' } -> `ArchiveResult' #}
{# fun archive_write_get_bytes_per_block as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_write_set_bytes_in_last_block as ^ { `ArchivePtr', `CInt' } -> `ArchiveResult' #}
{# fun archive_write_get_bytes_in_last_block as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_write_set_skip_file as ^ { `ArchivePtr', `LaInt64', `LaInt64' } -> `ArchiveResult' #}
{# fun archive_write_add_filter as ^ { `ArchivePtr', `ArchiveFilter' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_by_name as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_b64encode as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_bzip2 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_compress as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_grzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_lrzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_lz4 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_lzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_lzma as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_lzop as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_none as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_program as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_uuencode as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_xz as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_add_filter_zstd as ^ { `ArchivePtr' } -> `ArchiveResult' #}

{# fun archive_write_set_format as ^ { `ArchivePtr', `ArchiveFormat' } -> `ArchiveResult' #}
{# fun archive_write_set_format_by_name as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_format_7zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_ar_bsd as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_ar_svr4 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_cpio as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_cpio_newc as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_gnutar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_mtree as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_mtree_classic as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_pax as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_pax_restricted as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_raw as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_shar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_shar_dump as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_ustar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_v7tar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_warc as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_xar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_set_format_filter_by_ext as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_format_filter_by_ext_def as ^ { `ArchivePtr', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_write_zip_set_compression_deflate as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_zip_set_compression_store as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_open as ^ { `ArchivePtr'
                               , castPtr `Ptr a'
                               , castFunPtr `FunPtr (ArchiveOpenCallbackRaw a)'
                               , castFunPtr `FunPtr (ArchiveWriteCallback a b)'
                               , castFunPtr `FunPtr (ArchiveCloseCallbackRaw a)'
                               } -> `ArchiveResult' #}
{# fun archive_write_open_fd as ^ { `ArchivePtr', coerce `Fd' } -> `ArchiveResult' #}
{# fun archive_write_open_filename as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_open_filename_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_write_open_FILE as ^ { `ArchivePtr', `FilePtr' } -> `ArchiveResult' #}
{# fun archive_write_open_memory as ^ { `ArchivePtr', castPtr `Ptr a' , `CSize', alloca- `CSize' peek* } -> `ArchiveResult' #}

{# fun archive_write_header as ^ { `ArchivePtr', `ArchiveEntryPtr' } -> `ArchiveResult' #}
{# fun archive_write_data as ^ { `ArchivePtr', castPtr `Ptr a', `CSize' } -> `LaSSize' #}
{# fun archive_write_data_block as ^ { `ArchivePtr', castPtr `Ptr a', `CSize', `LaInt64' } -> `LaSSize' #}

{# fun archive_write_finish_entry as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_close as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_fail as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_free as ^ { `ArchivePtr' } -> `ArchiveResult' #}

{# fun archive_write_set_format_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_filter_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_option as ^ { `ArchivePtr', `CString', `CString', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_options as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}

{# fun archive_write_set_passphrase as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_write_set_passphrase_callback as ^ { `ArchivePtr', castPtr `Ptr a', castFunPtr `FunPtr (ArchivePassphraseCallback a)' } -> `ArchiveResult' #}
{# fun archive_write_disk_set_options as ^ { `ArchivePtr', coerce `Flags' } -> `ArchiveResult' #}

{# fun archive_write_disk_new as ^ {} -> `ArchivePtr' #}
{# fun archive_write_disk_set_skip_file as ^ { `ArchivePtr', `LaInt64', `LaInt64' } -> `ArchiveResult' #}

{# fun archive_write_disk_set_standard_lookup as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_write_disk_set_group_lookup as ^ { `ArchivePtr'
                                                , castPtr `Ptr a'
                                                , castFunPtr `FunPtr (Ptr a -> CString -> LaInt64 -> IO LaInt64)'
                                                , castFunPtr `FunPtr (Ptr a -> IO ())'
                                                } -> `ArchiveResult' #}
{# fun archive_write_disk_set_user_lookup as ^ { `ArchivePtr'
                                               , castPtr `Ptr a'
                                               , castFunPtr `FunPtr (Ptr a -> CString -> LaInt64 -> IO LaInt64)'
                                               , castFunPtr `FunPtr (Ptr a -> IO ())'
                                               } -> `ArchiveResult' #}
{# fun archive_write_disk_gid as ^ { `ArchivePtr', `CString', `LaInt64' } -> `LaInt64' #}
{# fun archive_write_disk_uid as ^ { `ArchivePtr', `CString', `LaInt64' } -> `LaInt64' #}

{# fun archive_read_disk_new as ^ {} -> `ArchivePtr' #}
{# fun archive_read_disk_set_symlink_logical as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_set_symlink_physical as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_set_symlink_hybrid as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_entry_from_file as ^ { `ArchivePtr', `ArchiveEntryPtr', coerce `Fd', `StatPtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_gname as ^ { `ArchivePtr', `LaInt64' } -> `CString' #}
{# fun archive_read_disk_uname as ^ { `ArchivePtr', `LaInt64' } -> `CString' #}
{# fun archive_read_disk_set_standard_lookup as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_set_gname_lookup as ^ { `ArchivePtr'
                                               , castPtr `Ptr a'
                                               , castFunPtr `FunPtr (Ptr a -> LaInt64 -> IO CString )'
                                               , castFunPtr `FunPtr (Ptr a -> IO ())'
                                               } -> `ArchiveResult' #}
{# fun archive_read_disk_set_uname_lookup as ^ { `ArchivePtr'
                                               , castPtr `Ptr a'
                                               , castFunPtr `FunPtr (Ptr a -> LaInt64 -> IO CString )'
                                               , castFunPtr `FunPtr (Ptr a -> IO ())'
                                               } -> `ArchiveResult' #}
{# fun archive_read_disk_open as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_read_disk_open_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_read_disk_descend as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_can_descend as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_read_disk_current_filesystem as ^ { `ArchivePtr' } -> `CInt' #}
{# fun archive_read_disk_current_filesystem_is_synthetic as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_read_disk_current_filesystem_is_remote as ^ { `ArchivePtr' } -> `Bool' #}
{# fun archive_read_disk_set_atime_restored as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_disk_set_behavior as ^ { `ArchivePtr', coerce `Flags' } -> `ArchiveResult' #}

{# fun archive_read_disk_set_matching as ^ { `ArchivePtr'
                                           , `ArchivePtr'
                                           , castFunPtr `FunPtr (ArchivePtr -> Ptr a -> ArchiveEntryPtr -> IO ())'
                                           , castPtr `Ptr a'
                                           } -> `ArchiveResult' #}
{# fun archive_read_disk_set_metadata_filter_callback as ^ { `ArchivePtr'
                                                           , castFunPtr `FunPtr (ArchivePtr -> Ptr a -> ArchiveEntry -> IO CInt)'
                                                           , castPtr `Ptr a'
                                                           } -> `ArchiveResult' #}

{# fun archive_free as ^ { `ArchivePtr' } -> `ArchiveResult' #}

{# fun archive_match_include_gname_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_gname as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_uname_w as ^ { `ArchivePtr', `CWString' } -> `ArchiveResult' #}
{# fun archive_match_include_uname as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_match_include_gid as ^ { `ArchivePtr', coerce `Id' } -> `ArchiveResult' #}
{# fun archive_match_include_uid as ^ { `ArchivePtr', coerce `Id' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_all as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_bzip2 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_compress as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_gzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_grzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lrzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lz4 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzma as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_lzop as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_none as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_program as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_program_signature as ^ { `ArchivePtr', `CString', castPtr `Ptr a', coerce `CSize' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_rpm as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_uu as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_filter_xz as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_7zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_all as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_ar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_by_code as ^ { `ArchivePtr', `CInt' } -> `ArchiveResult' #}
{# fun archive_read_support_format_cab as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_cpio as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_empty as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_gnutar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_iso9660 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_lha as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_mtree as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_rar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_rar5 as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_raw as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_tar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_warc as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_xar as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip_streamable as ^ { `ArchivePtr' } -> `ArchiveResult' #}
{# fun archive_read_support_format_zip_seekable as ^ { `ArchivePtr' } -> `ArchiveResult' #}

{# fun archive_read_set_format as ^ { `ArchivePtr', `ArchiveFormat' } -> `ArchiveResult' #}
{# fun archive_read_append_filter as ^ { `ArchivePtr', `CInt' } -> `ArchiveResult' #}
{# fun archive_read_append_filter_program as ^ { `ArchivePtr', `CString' } -> `ArchiveResult' #}
{# fun archive_read_append_filter_program_signature as ^ { `ArchivePtr', `CString', castPtr `Ptr a', `CSize' } -> `ArchiveResult' #}

{# fun archive_errno as ^ { `ArchivePtr' } -> `ArchiveResult' #}
