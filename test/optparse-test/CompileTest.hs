{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CompileTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import GHCup.GHC
import Data.Versions
import GHCup.Types
import URI.ByteString.QQ
import qualified GHCup.OptParse.Compile as GHC (GHCCompileOptions(..))
import qualified GHCup.OptParse.Compile as HLS (HLSCompileOptions(..))


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
  [ ("compile ghc -v 9.4.5 -b 9.2.8", baseOptions)
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
  , ("compile ghc --remote-source-dist https://gitlab.haskell.org/ghc/ghc.git -b 9.2.8", mkDefaultGHCCompileOptions
        (RemoteDist [uri|https://gitlab.haskell.org/ghc/ghc.git|])
        (Left $ mkVersion' "9.2.8")
    )
  , (baseCmd <> "-j20", baseOptions{GHC.jobs = Just 20})
  , (baseCmd <> "--jobs 10", baseOptions{GHC.jobs = Just 10})
  , (baseCmd <> "-c build.mk", baseOptions{GHC.buildConfig = Just "build.mk"})
  , (baseCmd <> "--config build.mk", baseOptions{GHC.buildConfig = Just "build.mk"})
  , (baseCmd <> "--patch file:///example.patch", baseOptions{GHC.patches = Just $ Right [[uri|file:///example.patch|]]})
  , (baseCmd <> "-p patch_dir", baseOptions{GHC.patches = Just (Left "patch_dir")})
  , (baseCmd <> "--patchdir patch_dir", baseOptions{GHC.patches = Just (Left "patch_dir")})
  , (baseCmd <> "-x armv7-unknown-linux-gnueabihf", baseOptions{GHC.crossTarget = Just "armv7-unknown-linux-gnueabihf"})
  , (baseCmd <> "--cross-target armv7-unknown-linux-gnueabihf", baseOptions{GHC.crossTarget = Just "armv7-unknown-linux-gnueabihf"})
  , (baseCmd <> "-- --enable-unregisterised", baseOptions{GHC.addConfArgs = ["--enable-unregisterised"]})
  , (baseCmd <> "--set", baseOptions{GHC.setCompile = True})
  , (baseCmd <> "-o 9.4.5-p1", baseOptions{GHC.ovewrwiteVer = Just $ mkVersion' "9.4.5-p1"})
  , (baseCmd <> "--overwrite-version 9.4.5-p1", baseOptions{GHC.ovewrwiteVer = Just $ mkVersion' "9.4.5-p1"})
  , (baseCmd <> "-f make", baseOptions{GHC.buildFlavour = Just "make"})
  , (baseCmd <> "--flavour make", baseOptions{GHC.buildFlavour = Just "make"})
  , (baseCmd <> "--hadrian", baseOptions{GHC.hadrian = True})
  , (baseCmd <> "-i /tmp/out_dir", baseOptions{GHC.isolateDir = Just "/tmp/out_dir"})
  , (baseCmd <> "--isolate /tmp/out_dir", baseOptions{GHC.isolateDir = Just "/tmp/out_dir"})
  ]
  where
    baseCmd = "compile ghc -v 9.4.5 -b 9.2.8 "
    baseOptions :: GHCCompileOptions
    baseOptions =
      mkDefaultGHCCompileOptions
        (SourceDist $ mkVersion' "9.4.5")
        (Left $ mkVersion' "9.2.8")

compileHlsCheckList :: [(String, CompileCommand)]
compileHlsCheckList = []

compileParseWith :: [String] -> IO CompileCommand
compileParseWith args = do
  Compile a <- parseWith args
  pure a
