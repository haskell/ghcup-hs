{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}


module GHCup.Platform where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Bash
import           GHCup.Utils.File
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions
import           HPath
import           HPath.IO
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.Info
import           Text.Regex.Posix

import qualified Data.Text                     as T

    --------------------------
    --[ Platform detection ]--
    --------------------------


-- | Get the full platform request, consisting of architecture, distro, ...
platformRequest :: (MonadLogger m, MonadCatch m, MonadIO m)
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


getPlatform :: (MonadLogger m, MonadCatch m, MonadIO m)
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
        ( either (const Nothing) Just
          . versioning
          -- TODO: maybe do this somewhere else
          . getMajorVersion
          . decUTF8Safe
          )
          <$> getDarwinVersion
      pure $ PlatformResult { _platform = Darwin, _distroVersion = ver }
    "freebsd" -> do
      ver <-
        (either (const Nothing) Just . versioning . decUTF8Safe)
          <$> getFreeBSDVersion
      pure $ PlatformResult { _platform = FreeBSD, _distroVersion = ver }
    what -> throwE $ NoCompatiblePlatform what
  lift $ $(logDebug) [i|Identified Platform as: #{pfr}|]
  pure pfr
 where
  getMajorVersion = T.intercalate "." . take 2 . T.split (== '.')
  getFreeBSDVersion =
    liftIO $ fmap _stdOut $ executeOut [rel|freebsd-version|] [] Nothing
  getDarwinVersion = liftIO $ fmap _stdOut $ executeOut [rel|sw_vers|]
                                                        ["-productVersion"]
                                                        Nothing


getLinuxDistro :: (MonadCatch m, MonadIO m)
               => Excepts '[DistroNotFound] m (LinuxDistro, Maybe Versioning)
getLinuxDistro = do
  -- TODO: don't do alternative on IO, because it hides bugs
  (name, ver) <- handleIO (\_ -> throwE DistroNotFound) $ liftIO $ asum
    [ try_os_release
    , try_lsb_release_cmd
    , try_lsb_release
    , try_redhat_release
    , try_debian_version
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
        | otherwise                -> UnknownLinux
  pure (distro, parsedVer)
 where
  hasWord t matches = foldr (\x y -> match (regex x) (T.unpack t) || y)
                            False
                            matches
   where
    regex x = makeRegexOpts compIgnoreCase execBlank ([s|\<|] ++ x ++ [s|\>|])

  os_release :: Path Abs
  os_release = [abs|/etc/os-release|]
  lsb_release :: Path Abs
  lsb_release = [abs|/etc/lsb-release|]
  lsb_release_cmd :: Path Rel
  lsb_release_cmd = [rel|lsb-release|]
  redhat_release :: Path Abs
  redhat_release = [abs|/etc/redhat-release|]
  debian_version :: Path Abs
  debian_version = [abs|/etc/debian_version|]

  try_os_release :: IO (Text, Maybe Text)
  try_os_release = do
    (Just name) <- getAssignmentValueFor os_release "NAME"
    ver         <- getAssignmentValueFor os_release "VERSION_ID"
    pure (T.pack name, fmap T.pack ver)

  try_lsb_release_cmd :: IO (Text, Maybe Text)
  try_lsb_release_cmd = do
    (Just _) <- findExecutable lsb_release_cmd
    name     <- fmap _stdOut $ executeOut lsb_release_cmd ["-si"] Nothing
    ver      <- fmap _stdOut $ executeOut lsb_release_cmd ["-sr"] Nothing
    pure (decUTF8Safe name, Just $ decUTF8Safe ver)

  try_lsb_release :: IO (Text, Maybe Text)
  try_lsb_release = do
    (Just name) <- getAssignmentValueFor lsb_release "DISTRIB_ID"
    ver         <- getAssignmentValueFor lsb_release "DISTRIB_RELEASE"
    pure (T.pack name, fmap T.pack ver)

  try_redhat_release :: IO (Text, Maybe Text)
  try_redhat_release = do
    t <- fmap decUTF8Safe' $ readFile redhat_release
    let nameRegex n =
          makeRegexOpts compIgnoreCase
                        execBlank
                        (([s|\<|] <> fS n <> [s|\>|] :: ByteString)) :: Regex
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
    ver <- readFile debian_version
    pure (T.pack "debian", Just . decUTF8Safe' $ ver)
