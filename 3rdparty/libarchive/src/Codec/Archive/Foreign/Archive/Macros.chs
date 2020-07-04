module Codec.Archive.Foreign.Archive.Macros ( archiveVersionNumberMacro
                                            , archiveVersionOnlyString
                                            , archiveVersionStringMacro
                                            , archiveReadFormatCapsNone
                                            , archiveReadFormatCapsEncryptData
                                            , archiveReadFormatCapsEncryptMetadata
                                            , archiveMatchMTime
                                            , archiveMatchCTime
                                            , archiveMatchNewer
                                            , archiveMatchOlder
                                            , archiveMatchEqual
                                            , archiveExtractOwner
                                            , archiveExtractPerm
                                            , archiveExtractNoOverwrite
                                            , archiveExtractUnlink
                                            , archiveExtractACL
                                            , archiveExtractFFlags
                                            , archiveExtractXattr
                                            , archiveExtractSecureSymlinks
                                            , archiveExtractSecureNoDotDot
                                            , archiveExtractTime
                                            , archiveExtractNoAutodir
                                            , archiveExtractSparse
                                            , archiveExtractMacMetadata
                                            , archiveExtractNoHfsCompression
                                            , archiveExtractHfsCompressionForced
                                            , archiveExtractSecureNoAbsolutePaths
                                            , archiveExtractClearNoChangeFFlags
                                            , archiveExtractNoOverwriteNewer
                                            , archiveReadDiskRestoreATime
                                            , archiveReadDiskHonorNoDump
                                            , archiveReadDiskMacCopyFile
                                            , archiveReadDiskNoTraverseMounts
                                            , archiveReadDiskNoXattr
                                            , archiveReadDiskNoAcl
                                            , archiveReadDiskNoFFlags
                                            -- * Conversion functions
                                            , encryptionResult
                                            ) where

import Codec.Archive.Types
import Data.Bits (Bits (..))
import Foreign.C.Types

#include <archive.h>

archiveVersionNumberMacro :: Int
archiveVersionNumberMacro = {# const ARCHIVE_VERSION_NUMBER #}

archiveVersionOnlyString :: String
archiveVersionOnlyString = {# const ARCHIVE_VERSION_ONLY_STRING #}

archiveVersionStringMacro :: String
archiveVersionStringMacro = {# const ARCHIVE_VERSION_STRING #}

-- Extraction flags
archiveExtractOwner :: Flags
archiveExtractOwner = Flags {# const ARCHIVE_EXTRACT_OWNER #}

archiveExtractPerm :: Flags
archiveExtractPerm = Flags {# const ARCHIVE_EXTRACT_PERM #}

archiveExtractTime :: Flags
archiveExtractTime = Flags {# const ARCHIVE_EXTRACT_TIME #}

archiveExtractNoOverwrite :: Flags
archiveExtractNoOverwrite = Flags {# const ARCHIVE_EXTRACT_NO_OVERWRITE #}

archiveExtractUnlink :: Flags
archiveExtractUnlink = Flags {# const ARCHIVE_EXTRACT_UNLINK #}

archiveExtractACL :: Flags
archiveExtractACL = Flags {# const ARCHIVE_EXTRACT_ACL #}

archiveExtractFFlags :: Flags
archiveExtractFFlags = Flags {# const ARCHIVE_EXTRACT_FFLAGS #}

archiveExtractXattr :: Flags
archiveExtractXattr = Flags {# const ARCHIVE_EXTRACT_XATTR #}

archiveExtractSecureSymlinks :: Flags
archiveExtractSecureSymlinks = Flags {# const ARCHIVE_EXTRACT_SECURE_SYMLINKS #}

archiveExtractSecureNoDotDot :: Flags
archiveExtractSecureNoDotDot = Flags {# const ARCHIVE_EXTRACT_SECURE_NODOTDOT #}

archiveExtractNoAutodir :: Flags
archiveExtractNoAutodir = Flags {# const ARCHIVE_EXTRACT_NO_AUTODIR #}

archiveExtractNoOverwriteNewer :: Flags
archiveExtractNoOverwriteNewer = Flags {# const ARCHIVE_EXTRACT_NO_OVERWRITE_NEWER #}

archiveExtractSparse :: Flags
archiveExtractSparse = Flags {# const ARCHIVE_EXTRACT_SPARSE #}

archiveExtractMacMetadata :: Flags
archiveExtractMacMetadata = Flags {# const ARCHIVE_EXTRACT_MAC_METADATA #}

archiveExtractNoHfsCompression :: Flags
archiveExtractNoHfsCompression = Flags {# const ARCHIVE_EXTRACT_NO_HFS_COMPRESSION #}

archiveExtractHfsCompressionForced :: Flags
archiveExtractHfsCompressionForced = Flags {# const ARCHIVE_EXTRACT_HFS_COMPRESSION_FORCED #}

archiveExtractSecureNoAbsolutePaths :: Flags
archiveExtractSecureNoAbsolutePaths = Flags {# const ARCHIVE_EXTRACT_SECURE_NOABSOLUTEPATHS #}

archiveExtractClearNoChangeFFlags :: Flags
archiveExtractClearNoChangeFFlags = Flags {# const ARCHIVE_EXTRACT_CLEAR_NOCHANGE_FFLAGS #}

encryptionResult :: CInt -> ArchiveEncryption
encryptionResult 0                                                        = NoEncryption
encryptionResult 1                                                        = HasEncryption
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_UNSUPPORTED #}) = EncryptionUnsupported
encryptionResult ({# const ARCHIVE_READ_FORMAT_ENCRYPTION_DONT_KNOW #})   = EncryptionUnknown
encryptionResult _                                                        = error "Should not happen."

archiveReadFormatCapsNone :: ArchiveCapabilities
archiveReadFormatCapsNone = ArchiveCapabilities {# const ARCHIVE_READ_FORMAT_CAPS_NONE #}

(<<) :: Bits a => a -> Int -> a
m << n = m `shift` n

archiveReadFormatCapsEncryptData :: ArchiveCapabilities
archiveReadFormatCapsEncryptData = ArchiveCapabilities ({# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #})

archiveReadFormatCapsEncryptMetadata :: ArchiveCapabilities
archiveReadFormatCapsEncryptMetadata = ArchiveCapabilities ({# const ARCHIVE_READ_FORMAT_CAPS_ENCRYPT_DATA #})

archiveReadDiskRestoreATime :: ReadDiskFlags
archiveReadDiskRestoreATime = ReadDiskFlags {# const ARCHIVE_READDISK_RESTORE_ATIME #}

archiveReadDiskHonorNoDump :: ReadDiskFlags
archiveReadDiskHonorNoDump = ReadDiskFlags {# const ARCHIVE_READDISK_HONOR_NODUMP #}

archiveReadDiskMacCopyFile :: ReadDiskFlags
archiveReadDiskMacCopyFile = ReadDiskFlags {# const ARCHIVE_READDISK_MAC_COPYFILE #}

archiveReadDiskNoTraverseMounts :: ReadDiskFlags
archiveReadDiskNoTraverseMounts = ReadDiskFlags {# const ARCHIVE_READDISK_NO_TRAVERSE_MOUNTS #}

archiveReadDiskNoXattr :: ReadDiskFlags
archiveReadDiskNoXattr = ReadDiskFlags {# const ARCHIVE_READDISK_NO_XATTR #}

-- | @since 2.1.1.0
archiveReadDiskNoAcl :: ReadDiskFlags
archiveReadDiskNoAcl = ReadDiskFlags {# const ARCHIVE_READDISK_NO_ACL #}

-- | @since 2.1.1.0
archiveReadDiskNoFFlags :: ReadDiskFlags
archiveReadDiskNoFFlags = ReadDiskFlags {# const ARCHIVE_READDISK_NO_FFLAGS #}

archiveMatchMTime :: TimeFlag
archiveMatchMTime = TimeFlag {# const ARCHIVE_MATCH_MTIME #}

archiveMatchCTime :: TimeFlag
archiveMatchCTime = TimeFlag {# const ARCHIVE_MATCH_CTIME #}

archiveMatchNewer :: TimeFlag
archiveMatchNewer = TimeFlag {# const ARCHIVE_MATCH_NEWER #}

archiveMatchOlder :: TimeFlag
archiveMatchOlder = TimeFlag {# const ARCHIVE_MATCH_OLDER #}

archiveMatchEqual :: TimeFlag
archiveMatchEqual = TimeFlag {# const ARCHIVE_MATCH_EQUAL #}
