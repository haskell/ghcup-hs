{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskellQuotes #-}


{-|
Module      : GHCup.Platform
Description : Retrieving platform information
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Platform where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prelude.String.QQ
import           GHCup.Prelude.Version.QQ
import           GHCup.Prelude.MegaParsec

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.Info
import           System.OsRelease
import           System.Exit
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix

import qualified Text.Megaparsec               as MP

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Void
import qualified Data.List                     as L




    --------------------------
    --[ Platform detection ]--
    --------------------------


-- | Get the full platform request, consisting of architecture, distro, ...
platformRequest :: (MonadReader env m, Alternative m, MonadFail m, HasLog env, MonadCatch m, MonadIO m)
                => Excepts
                     '[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound]
                     m
                     PlatformRequest
platformRequest = do
  (PlatformResult rp rv) <- liftE getPlatform
  ar                     <- lE getArchitecture
  pure $ PlatformRequest ar rp rv


getArchitecture :: Either NoCompatibleArch Architecture
getArchitecture = case arch of
  "x86_64"      -> Right A_64
  "i386"        -> Right A_32
  "powerpc"     -> Right A_PowerPC
  "powerpc64"   -> Right A_PowerPC64
  "powerpc64le" -> Right A_PowerPC64
  "sparc"       -> Right A_Sparc
  "sparc64"     -> Right A_Sparc64
  "arm"         -> Right A_ARM
  "aarch64"     -> Right A_ARM64
  what          -> Left (NoCompatibleArch what)


getPlatform :: (Alternative m, MonadReader env m, HasLog env, MonadCatch m, MonadIO m, MonadFail m)
            => Excepts
                 '[NoCompatiblePlatform, DistroNotFound]
                 m
                 PlatformResult
getPlatform = do
  pfr <- case os of
    "linux" -> do
      (distro, ver) <- liftE getLinuxDistro
      pure $ PlatformResult { _platform = Linux distro, _distroVersion = ver }
    "darwin" -> do
      ver <-
        either (const Nothing) Just
          . versioning
          -- TODO: maybe do this somewhere else
          . decUTF8Safe'
        <$> getDarwinVersion
      pure $ PlatformResult { _platform = Darwin, _distroVersion = ver }
    "freebsd" -> do
      ver <-
        either (const Nothing) Just . versioning . decUTF8Safe'
          <$> getFreeBSDVersion
      pure $ PlatformResult { _platform = FreeBSD, _distroVersion = ver }
    "mingw32" -> pure PlatformResult { _platform = Windows, _distroVersion = Nothing }
    what -> throwE $ NoCompatiblePlatform what
  lift $ logDebug $ "Identified Platform as: " <> T.pack (prettyShow pfr)
  pure pfr
 where
  getFreeBSDVersion = lift $ fmap _stdOut $ executeOut "freebsd-version" [] Nothing
  getDarwinVersion = lift $ fmap _stdOut $ executeOut "sw_vers"
                                                        ["-productVersion"]
                                                        Nothing


getLinuxDistro :: (Alternative m, MonadCatch m, MonadIO m, MonadFail m)
               => Excepts '[DistroNotFound] m (LinuxDistro, Maybe Versioning)
getLinuxDistro = do
  -- TODO: don't do alternative on IO, because it hides bugs
  (name, ver) <- handleIO (\_ -> throwE DistroNotFound) $ lift $ asum
    [ liftIO try_os_release
    , try_lsb_release_cmd
    , liftIO try_redhat_release
    , liftIO try_debian_version
    ]
  let parsedVer = ver >>= either (const Nothing) Just . versioning
      distro    = if
        | hasWord name ["debian"]  -> Debian
        | hasWord name ["ubuntu"]  -> Ubuntu
        | hasWord name ["linuxmint", "Linux Mint"] -> Mint
        | hasWord name ["fedora"]  -> Fedora
        | hasWord name ["centos"]  -> CentOS
        | hasWord name ["Red Hat"] -> RedHat
        | hasWord name ["alpine"]  -> Alpine
        | hasWord name ["exherbo"] -> Exherbo
        | hasWord name ["gentoo"]  -> Gentoo
        | hasWord name ["amazonlinux", "Amazon Linux"] -> AmazonLinux
        | hasWord name ["rocky", "Rocky Linux"] -> Rocky
        -- https://github.com/void-linux/void-packages/blob/master/srcpkgs/base-files/files/os-release
        | hasWord name ["void", "Void Linux"] -> Void
        | otherwise                -> UnknownLinux
  pure (distro, parsedVer)
 where
  hasWord t = any (\x -> match (regex x) (T.unpack t))
   where
    regex x = makeRegexOpts compIgnoreCase execBlank ([s|\<|] ++ x ++ [s|\>|])

  lsb_release_cmd :: FilePath
  lsb_release_cmd = "lsb-release"
  redhat_release :: FilePath
  redhat_release = "/etc/redhat-release"
  debian_version :: FilePath
  debian_version = "/etc/debian_version"

  try_os_release :: IO (Text, Maybe Text)
  try_os_release = do
    Just OsRelease{ name = name, version_id = version_id } <-
      fmap osRelease <$> parseOsRelease
    pure (T.pack name, fmap T.pack version_id)

  try_lsb_release_cmd :: (MonadFail m, MonadIO m)
                      => m (Text, Maybe Text)
  try_lsb_release_cmd = do
    (Just _) <- liftIO $ findExecutable lsb_release_cmd
    name     <- fmap _stdOut $ executeOut lsb_release_cmd ["-si"] Nothing
    ver      <- fmap _stdOut $ executeOut lsb_release_cmd ["-sr"] Nothing
    pure (decUTF8Safe' name, Just $ decUTF8Safe' ver)

  try_redhat_release :: IO (Text, Maybe Text)
  try_redhat_release = do
    t <- T.readFile redhat_release
    let nameRegex n =
          makeRegexOpts compIgnoreCase
                        execBlank
                        ([s|\<|] <> fS n <> [s|\>|] :: ByteString) :: Regex
    let verRegex =
          makeRegexOpts compIgnoreCase
                        execBlank
                        ([s|\<([0-9])+(.([0-9])+)*\>|] :: ByteString) :: Regex
    let nameRe n =
          fromEmpty . match (nameRegex n) $ T.unpack t :: Maybe String
        verRe = fromEmpty . match verRegex $ T.unpack t :: Maybe String
    (Just name) <- pure
      (nameRe "CentOS" <|> nameRe "Fedora" <|> nameRe "Red Hat")
    pure (T.pack name, fmap T.pack verRe)
   where
    fromEmpty :: String -> Maybe String
    fromEmpty "" = Nothing
    fromEmpty s' = Just s'

  try_debian_version :: IO (Text, Maybe Text)
  try_debian_version = do
    ver <- T.readFile debian_version
    pure (T.pack "debian", Just ver)


getStackGhcBuilds :: (MonadReader env m, HasLog env, MonadIO m)
                  => PlatformResult
                  -> Excepts '[ParseError, NoCompatiblePlatform, DistroNotFound, ProcessError] m [String]
getStackGhcBuilds PlatformResult{..} = do
    case _platform of
      Linux _ -> do
        -- Some systems don't have ldconfig in the PATH, so make sure to look in
        -- /sbin and /usr/sbin as well
        sbinEnv <- liftIO $ addToPath sbinDirs False
        ldConfig <- lift $ executeOut' "ldconfig" ["-p"] Nothing (Just sbinEnv)
        firstWords <- case ldConfig of
                        CapturedProcess ExitSuccess so _ ->
                          pure . mapMaybe (listToMaybe . T.words) . T.lines . T.pack . stripNewlineEnd . T.unpack . decUTF8Safe' $ so
                        CapturedProcess (ExitFailure _) _ _ ->
                          -- throwE $ NonZeroExit c "ldconfig" ["-p" ]
                          pure []
        let checkLib :: (MonadReader env m, HasLog env, MonadIO m) => String -> m Bool
            checkLib lib
              | libT `elem` firstWords = do
                  logDebug $ "Found shared library " <> libT <> " in 'ldconfig -p' output"
                  pure True
              | isWindows =
                  -- Cannot parse /usr/lib on Windows
                  pure False
              | otherwise = hasMatches lib usrLibDirs
              -- This is a workaround for the fact that libtinfo.so.x doesn't
              -- appear in the 'ldconfig -p' output on Arch or Slackware even
              -- when it exists. There doesn't seem to be an easy way to get the
              -- true list of directories to scan for shared libs, but this
              -- works for our particular cases.
             where
              libT = T.pack lib

            hasMatches :: (MonadReader env m, HasLog env, MonadIO m) => String -> [FilePath] -> m Bool
            hasMatches lib dirs = do
              matches <- filterM (liftIO . doesFileExist . (</> lib)) dirs
              case matches of
                [] -> logDebug ("Did not find shared library " <> libT) >> pure False
                (path:_) -> logDebug ("Found shared library " <> libT <> " in " <> T.pack path) >> pure True
             where
              libT = T.pack lib

            getLibc6Version :: MonadIO m
                            => Excepts '[ParseError, ProcessError] m Version
            getLibc6Version = do
              CapturedProcess{..} <- lift $ executeOut "ldd" ["--version"] Nothing
              case _exitCode of
                ExitSuccess -> either (throwE . ParseError . show) pure
                                 . MP.parse lddVersion "" . T.pack . stripNewlineEnd . T.unpack . decUTF8Safe' $ _stdOut
                ExitFailure c -> throwE $ NonZeroExit c "ldd" ["--version" ]

            -- Assumes the first line of ldd has the format:
            --
            -- ldd (...) nn.nn
            --
            -- where nn.nn corresponds to the version of libc6.
            lddVersion :: MP.Parsec Void Text Version
            lddVersion = do
              skipWhile (/= ')')
              skip (== ')')
              skipSpace
              version'

        hasMusl <- hasMatches relFileLibcMuslx86_64So1 libDirs
        mLibc6Version <- veitherToEither <$> runE getLibc6Version
        case mLibc6Version of
          Right libc6Version -> logDebug $ "Found shared library libc6 in version: " <> prettyVer libc6Version
          Left _ -> logDebug "Did not find a version of shared library libc6."
        let hasLibc6_2_32 = either (const False) (>= [vver|2.32|]) mLibc6Version
        hastinfo5 <- checkLib relFileLibtinfoSo5
        hastinfo6 <- checkLib relFileLibtinfoSo6
        hasncurses6 <- checkLib relFileLibncurseswSo6
        hasgmp5 <- checkLib relFileLibgmpSo10
        hasgmp4 <- checkLib relFileLibgmpSo3
        let libComponents = if hasMusl
              then
                [ ["musl"] ]
              else
                concat
                  [ if hastinfo6 && hasgmp5
                    then
                      if hasLibc6_2_32
                      then [["tinfo6"]]
                      else [["tinfo6-libc6-pre232"]]
                    else [[]]
                  , [ [] | hastinfo5 && hasgmp5 ]
                  , [ ["ncurses6"] | hasncurses6 && hasgmp5 ]
                  , [ ["gmp4"] | hasgmp4 ]
                  ]
        pure $ map
          (\c -> case c of
            [] -> []
            _ -> L.intercalate "-" c)
          libComponents
      FreeBSD ->
        case _distroVersion of
          Just fVer
            | fVer >= [vers|12|] -> pure []
          _ -> pure ["ino64"]
      Darwin  -> pure []
      Windows -> pure []
 where

  relFileLibcMuslx86_64So1 :: FilePath
  relFileLibcMuslx86_64So1 = "libc.musl-x86_64.so.1"
  libDirs :: [FilePath]
  libDirs = ["/lib", "/lib64"]
  usrLibDirs :: [FilePath]
  usrLibDirs = ["/usr/lib", "/usr/lib64"]
  sbinDirs :: [FilePath]
  sbinDirs = ["/sbin", "/usr/sbin"]
  relFileLibtinfoSo5 :: FilePath
  relFileLibtinfoSo5 = "libtinfo.so.5"
  relFileLibtinfoSo6 :: FilePath
  relFileLibtinfoSo6 = "libtinfo.so.6"
  relFileLibncurseswSo6 :: FilePath
  relFileLibncurseswSo6 = "libncursesw.so.6"
  relFileLibgmpSo10 :: FilePath
  relFileLibgmpSo10 = "libgmp.so.10"
  relFileLibgmpSo3 :: FilePath
  relFileLibgmpSo3 = "libgmp.so.3"

getStackOSKey :: Monad m => PlatformRequest -> Excepts '[UnsupportedSetupCombo] m String
getStackOSKey PlatformRequest { .. } =
  case (_rArch, _rPlatform) of
    (A_32   , Linux _) -> pure "linux32"
    (A_64   , Linux _) -> pure "linux64"
    (A_32   , Darwin ) -> pure "macosx"
    (A_64   , Darwin ) -> pure "macosx"
    (A_32   , FreeBSD) -> pure "freebsd32"
    (A_64   , FreeBSD) -> pure "freebsd64"
    (A_32   , Windows) -> pure "windows32"
    (A_64   , Windows) -> pure "windows64"
    (A_ARM  , Linux _) -> pure "linux-armv7"
    (A_ARM64, Linux _) -> pure "linux-aarch64"
    (A_Sparc, Linux _) -> pure "linux-sparc"
    (A_ARM64, Darwin ) -> pure "macosx-aarch64"
    (A_ARM64, FreeBSD) -> pure "freebsd-aarch64"
    (arch', os') -> throwE $ UnsupportedSetupCombo arch' os'

getStackPlatformKey :: (MonadReader env m, MonadFail m, HasLog env, MonadCatch m, MonadIO m)
                    => PlatformRequest
                    -> Excepts '[UnsupportedSetupCombo, ParseError, NoCompatiblePlatform, NoCompatibleArch, DistroNotFound, ProcessError] m [String]
getStackPlatformKey pfreq@PlatformRequest{..} = do
  osKey <- liftE  $ getStackOSKey pfreq
  builds <- liftE $ getStackGhcBuilds (PlatformResult _rPlatform _rVersion)
  let builds' = (\build -> if null build then osKey else osKey <> "-" <> build) <$> builds
  logDebug $ "Potential GHC builds: " <> mconcat (L.intersperse ", " $ fmap T.pack builds')
  pure builds'

