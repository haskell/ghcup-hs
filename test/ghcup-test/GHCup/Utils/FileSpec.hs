module GHCup.Utils.FileSpec where

import           GHCup.Prelude.File

import           Conduit
import           Data.List
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe

import           Test.Hspec



spec :: Spec
spec = do
  describe "GHCup.Utils.File" $ do
    it "getDirectoryContentsRecursiveUnsafe" $ do
      l1 <- sort <$>  runResourceT (sourceToList $ getDirectoryContentsRecursiveUnsafe "lib")
      l2 <- sort <$> getDirectoryContentsRecursiveLazy "lib"
      not (null l1) `shouldBe` True
      not (null l2) `shouldBe` True
      l1 `shouldBe` l2


getDirectoryContentsRecursiveLazy :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveLazy topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False


