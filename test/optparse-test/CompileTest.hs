{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module CompileTest where

import Test.Tasty
import GHCup.OptParse
import Utils
import Data.Versions
import GHCup.Types
import URI.ByteString.QQ
import qualified GHCup.OptParse.Compile as GHC (GHCCompileOptions(..))
import qualified GHCup.OptParse.Compile as HLS (HLSCompileOptions(..))
import GHCup.GHC as GHC
import GHCup.HLS as HLS


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
    Nothing
    Nothing

mkDefaultHLSCompileOptions :: HLSVer -> [ToolVersion] -> HLSCompileOptions
mkDefaultHLSCompileOptions target ghcs =
  HLSCompileOptions
    target
    Nothing
    True
    False
    (Left False)
    Nothing
    Nothing
    Nothing
    (Just $ Right [])
    ghcs
    []

compileGhcCheckList :: [(String, CompileCommand)]
compileGhcCheckList = mapSecond CompileGHC
  [ ("compile ghc -v 9.4.5 -b 9.2.8", baseOptions)
  , ("compile ghc -g a32db0b -b 9.2.8", mkDefaultGHCCompileOptions
        (GHC.GitDist $ GitBranch "a32db0b" Nothing)
        (Left $(verQ "9.2.8"))
    )
  , ("compile ghc -g a32db0b -b 9.2.8 -r https://gitlab.haskell.org/ghc/ghc.git",
        mkDefaultGHCCompileOptions
          (GHC.GitDist $ GitBranch "a32db0b" (Just "https://gitlab.haskell.org/ghc/ghc.git"))
          (Left $(verQ "9.2.8"))
    )
  , ("compile ghc -g a32db0b -r https://gitlab.haskell.org/ghc/ghc.git -b /usr/bin/ghc-9.2.2",
        mkDefaultGHCCompileOptions
          (GHC.GitDist $ GitBranch "a32db0b" (Just "https://gitlab.haskell.org/ghc/ghc.git"))
          (Right "/usr/bin/ghc-9.2.2")
    )
  , ("compile ghc --remote-source-dist https://gitlab.haskell.org/ghc/ghc.git -b 9.2.8", mkDefaultGHCCompileOptions
        (GHC.RemoteDist [uri|https://gitlab.haskell.org/ghc/ghc.git|])
        (Left $(verQ "9.2.8"))
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
  , (baseCmd <> "-o 9.4.5-p1", baseOptions{GHC.ovewrwiteVer = Just $(verQ "9.4.5-p1")})
  , (baseCmd <> "--overwrite-version 9.4.5-p1", baseOptions{GHC.ovewrwiteVer = Just $(verQ "9.4.5-p1")})
  , (baseCmd <> "-f make", baseOptions{GHC.buildFlavour = Just "make"})
  , (baseCmd <> "--flavour make", baseOptions{GHC.buildFlavour = Just "make"})
  , (baseCmd <> "--hadrian", baseOptions{GHC.buildSystem = Just Hadrian})
  , (baseCmd <> "--make", baseOptions{GHC.buildSystem = Just Make})
#ifdef IS_WINDOWS
  , (baseCmd <> "-i C:\\\\tmp\\out_dir", baseOptions{GHC.isolateDir = Just "C:\\\\tmp\\out_dir"})
  , (baseCmd <> "--isolate C:\\\\tmp\\out_dir", baseOptions{GHC.isolateDir = Just "C:\\\\tmp\\out_dir"})
#else
  , (baseCmd <> "-i /tmp/out_dir", baseOptions{GHC.isolateDir = Just "/tmp/out_dir"})
  , (baseCmd <> "--isolate /tmp/out_dir", baseOptions{GHC.isolateDir = Just "/tmp/out_dir"})
#endif
  ]
  where
    baseCmd :: String
    baseCmd = "compile ghc -v 9.4.5 -b 9.2.8 "

    baseOptions :: GHCCompileOptions
    baseOptions =
      mkDefaultGHCCompileOptions
        (GHC.SourceDist $(verQ "9.4.5"))
        (Left $(verQ "9.2.8"))

compileHlsCheckList :: [(String, CompileCommand)]
compileHlsCheckList = mapSecond CompileHLS
  [ ("compile hls -v 2.0.0.0 --ghc 9.2.8", baseOptions)
  , ("compile hls --version 2.0.0.0 --ghc 9.2.8", baseOptions)
  , ("compile hls -g a32db0b --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.GitDist $ GitBranch {ref = "a32db0b", repo = Nothing})
          [ghc928]
    )
  , ("compile hls --git-ref a32db0b --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.GitDist $ GitBranch {ref = "a32db0b", repo = Nothing})
          [ghc928]
    )
  , ("compile hls -g a32db0b -r https://github.com/haskell/haskell-language-server.git --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.GitDist $ GitBranch {ref = "a32db0b", repo = Just "https://github.com/haskell/haskell-language-server.git"})
          [ghc928]
    )
  , ("compile hls -g a32db0b --repository https://github.com/haskell/haskell-language-server.git --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.GitDist $ GitBranch {ref = "a32db0b", repo = Just "https://github.com/haskell/haskell-language-server.git"})
          [ghc928]
    )
  , ("compile hls --source-dist 2.0.0.0 --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.SourceDist $(verQ "2.0.0.0"))
          [ghc928]
    )
  , ("compile hls --remote-source-dist https://github.com/haskell/haskell-language-server/archive/refs/tags/2.0.0.1.tar.gz --ghc 9.2.8",
        mkDefaultHLSCompileOptions
          (HLS.RemoteDist [uri|https://github.com/haskell/haskell-language-server/archive/refs/tags/2.0.0.1.tar.gz|])
          [ghc928]
    )
  , ("compile hls -v 2.0.0.0 --ghc latest",
        mkDefaultHLSCompileOptions
          (HLS.HackageDist $(verQ "2.0.0.0"))
          [ToolTag Latest]
    )
  , (baseCmd <> "-j20", baseOptions{HLS.jobs = Just 20})
  , (baseCmd <> "--jobs 10", baseOptions{HLS.jobs = Just 10})
  , (baseCmd <> "--no-set", baseOptions{HLS.setCompile = False})
  , (baseCmd <> "--cabal-update", baseOptions{HLS.updateCabal = True})
  , (baseCmd <> "-o 2.0.0.0-p1", baseOptions{HLS.ovewrwiteVer = Right $(verQ "2.0.0.0-p1")})
  , (baseCmd <> "--overwrite-version 2.0.0.0-p1", baseOptions{HLS.ovewrwiteVer = Right $(verQ "2.0.0.0-p1")})
  , (baseCmd <> "--git-describe-version", baseOptions{HLS.ovewrwiteVer = Left True})
#ifdef IS_WINDOWS
  , (baseCmd <> "-i C:\\\\tmp\\out_dir", baseOptions{HLS.isolateDir = Just "C:\\\\tmp\\out_dir"})
  , (baseCmd <> "--isolate C:\\\\tmp\\out_dir", baseOptions{HLS.isolateDir = Just "C:\\\\tmp\\out_dir"})
#else
  , (baseCmd <> "-i /tmp/out_dir", baseOptions{HLS.isolateDir = Just "/tmp/out_dir"})
  , (baseCmd <> "--isolate /tmp/out_dir", baseOptions{HLS.isolateDir = Just "/tmp/out_dir"})
#endif
  , (baseCmd <> "--cabal-project file:///tmp/cabal.project", baseOptions{HLS.cabalProject = Just $ Right [uri|file:///tmp/cabal.project|]})
  , (baseCmd <> "--cabal-project cabal.ghc8107.project", baseOptions{HLS.cabalProject = Just $ Left "cabal.ghc8107.project"})
  , (baseCmd <> "--cabal-project-local file:///tmp/cabal.project.local", baseOptions{HLS.cabalProjectLocal = Just [uri|file:///tmp/cabal.project.local|]})
  , (baseCmd <> "--patch file:///example.patch", baseOptions{HLS.patches = Just $ Right [[uri|file:///example.patch|]]})
  , (baseCmd <> "-p patch_dir", baseOptions{HLS.patches = Just (Left "patch_dir")})
  , (baseCmd <> "--patchdir patch_dir", baseOptions{HLS.patches = Just (Left "patch_dir")})
  , (baseCmd <> "-- --enable-tests", baseOptions{HLS.cabalArgs = ["--enable-tests"]})
  ]
  where
    baseCmd :: String
    baseCmd = "compile hls -v 2.0.0.0 --ghc 9.2.8 "

    baseOptions :: HLSCompileOptions
    baseOptions =
      mkDefaultHLSCompileOptions
        (HLS.HackageDist $(verQ "2.0.0.0"))
        [ghc928]

    ghc928 :: ToolVersion
    ghc928 = GHCVersion $ GHCTargetVersion Nothing $(verQ "9.2.8")

compileParseWith :: [String] -> IO CompileCommand
compileParseWith args = do
  Compile a <- parseWith args
  pure a
