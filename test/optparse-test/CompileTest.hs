{-# LANGUAGE OverloadedStrings #-}

module CompileTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.GHC
import Data.Versions
import GHCup.Types


compileTests :: TestTree
compileTests = testGroup "compile"
  $ map (buildTestTree compileParseWith)
      [ ("ghc", compileGhcCheckList)
      , ("hls", compileHlsCheckList)
      ]

mkDefaultGHCCompileOptions :: GHCVer -> Either Version FilePath -> GHCCompileOptions
mkDefaultGHCCompileOptions target boot =
  GHCCompileOptions
    target
    boot
    Nothing
    Nothing
    (Just $ Right [])
    Nothing
    []
    False
    Nothing
    Nothing
    False
    Nothing

compileGhcCheckList :: [(String, CompileCommand)]
compileGhcCheckList = mapSecond CompileGHC
  [ ("compile ghc -v 9.4.5 -b 9.2.8", mkDefaultGHCCompileOptions
        (SourceDist $ mkVersion' "9.4.5")
        (Left $ mkVersion' "9.2.8")
    )
  , ("compile ghc -g a32db0b -b 9.2.8", mkDefaultGHCCompileOptions
        (GitDist $ GitBranch "a32db0b" Nothing)
        (Left $ mkVersion' "9.2.8")
    )
  , ("compile ghc -g a32db0b -b 9.2.8 -r https://gitlab.haskell.org/ghc/ghc.git",
        mkDefaultGHCCompileOptions
          (GitDist $ GitBranch "a32db0b" (Just "https://gitlab.haskell.org/ghc/ghc.git"))
          (Left $ mkVersion' "9.2.8")
    )
  , ("compile ghc -g a32db0b -r https://gitlab.haskell.org/ghc/ghc.git -b /usr/bin/ghc-9.2.2",
        mkDefaultGHCCompileOptions
          (GitDist $ GitBranch "a32db0b" (Just "https://gitlab.haskell.org/ghc/ghc.git"))
          (Right "/usr/bin/ghc-9.2.2")
    )
  ]

compileHlsCheckList :: [(String, CompileCommand)]
compileHlsCheckList = []

compileParseWith :: [String] -> IO CompileCommand
compileParseWith args = do
  Compile a <- parseWith args
  pure a
