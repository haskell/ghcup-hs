module Codec.Archive.Pack.Common ( mkEntry ) where

import           Codec.Archive.Types
import qualified Data.ByteString          as BS
import           System.PosixCompat.Files (FileStatus, fileGroup, fileMode, fileOwner, getFileStatus, isDirectory, isRegularFile, isSymbolicLink, linkCount,
                                           readSymbolicLink)

mkContent :: FilePath -> FileStatus -> IO EntryContent
mkContent fp status =
    let res = (isRegularFile status, isDirectory status, isSymbolicLink status, linkCount status)
    in

    case res of
        (True, False, False, 1) -> NormalFile <$> BS.readFile fp
        (True, False, False, _) -> pure $ Hardlink fp
        (False, True, False, _) -> pure Directory
        (False, False, True, _) -> Symlink <$> readSymbolicLink fp <*> pure SymlinkUndefined
        (_, _, _, _)            -> error "inconsistent read result"

mkEntry :: FilePath -> IO Entry
mkEntry fp = do
    status <- getFileStatus fp
    content' <- mkContent fp status
    pure $ Entry fp content' (fileMode status) (Ownership Nothing Nothing (fromIntegral $ fileOwner status) (fromIntegral $ fileGroup status)) Nothing

