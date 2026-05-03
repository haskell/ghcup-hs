{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.ArbitraryTypes where


import           GHCup.Types
import           GHCup.Types.Stack
import           GHCup.Types.JSON

import           Data.ByteString                ( ByteString )
import           Data.Char                      ( toLower )
import           Data.Versions
import           Data.List.NonEmpty
import           Data.Time.Calendar             ( Day(..) )
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT  ( ToADTArbitrary )
import           Test.QuickCheck.Arbitrary.Generic
import           URI.ByteString

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Lazy                as T
                                                ( toStrict )
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B


    -----------------
    --[ utilities ]--
    -----------------

intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal

genVer :: Gen (Int, Int, Int)
genVer =
  (\x y z -> (getPositive x, getPositive y, getPositive z))
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance ToADTArbitrary ToolDescription

instance ToADTArbitrary ToolInfo

instance ToADTArbitrary GHCupInfo



    ----------------------
    --[ base arbitrary ]--
    ----------------------

instance Arbitrary T.Text where
  arbitrary = fmap T.pack $ listOf $ elements ['a' .. 'z']
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary (NonEmpty Word) where
  arbitrary = fmap fromList $ listOf1 arbitrary

-- utf8 encoded bytestring
instance Arbitrary ByteString where
  arbitrary = fmap (E.encodeUtf8 . T.pack) $ listOf $ elements ['a' .. 'z']



    ---------------------
    --[ uri arbitrary ]--
    ---------------------

instance Arbitrary Scheme where
  arbitrary = elements [ Scheme "http", Scheme "https" ]

instance Arbitrary Host where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Port where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . fromIntegral <$> (chooseAny :: Gen Int)

instance Arbitrary (URIRef Absolute) where
  arbitrary =
    URI <$> arbitrary <*> pure Nothing <*> arbitrary <*> pure (Query []) <*> pure Nothing



    -------------------------
    --[ version arbitrary ]--
    -------------------------

instance Arbitrary Mess where
  arbitrary = do
    (x, y, z) <- genVer
    pure
      $ either (error . show) id
      $ mess (intToText x <> "." <> intToText y <> "." <> intToText z)

instance Arbitrary Version where
  arbitrary = do
    (x, y, z) <- genVer
    pure
      $ either (error . show) id
      $ version (intToText x <> "." <> intToText y <> "." <> intToText z)

instance Arbitrary SemVer where
  arbitrary = do
    (x, y, z) <- genVer
    pure
      $ either (error . show) id
      $ semver (intToText x <> "." <> intToText y <> "." <> intToText z)

instance Arbitrary PVP where
  arbitrary = do
    (x, y, z) <- genVer
    pure
      $ either (error . show) id
      $ pvp (intToText x <> "." <> intToText y <> "." <> intToText z)

instance Arbitrary Versioning where
  arbitrary = Ideal <$> arbitrary



    -----------------------
    --[ ghcup arbitrary ]--
    -----------------------

instance Arbitrary ToolDescription where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary ToolInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary EnvUnion where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary EnvSpec where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary ConfigSpec where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary MakeSpec where
  arbitrary = genericArbitrary
  shrink    = genericShrink


instance Arbitrary InstallationSpecInput where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary (NonEmpty InstallFileRule) where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary InstallFileRule where
  arbitrary = suchThat genericArbitrary pred'
   where
    pred' (InstallFileRule source dest) = safePath source && maybe True safePath dest
    pred' (InstallFilePatternRule fps) = all safePath fps
  shrink    = genericShrink

instance Arbitrary (SymlinkSpec String) where
  arbitrary = suchThat genericArbitrary pred'
   where
    pred' SymlinkSpec{..} =
         safePath _slTarget
      && safeFilename _slLinkName
      && maybe True safeFilename _slSetName
  shrink    = genericShrink

instance Arbitrary SymlinkInputSpec where
  arbitrary = suchThat genericArbitrary pred'
   where
    pred' SymlinkInputSpec{..} =
         safePath _slTarget
      && safeFilename _slLinkName
      && maybe True safeFilename _slSetName
    pred' SymlinkPatternSpec{..} =
         all safePath _slTargetPattern
      && all safePath _slTargetPatternIgnore
      && safeFilename _slLinkName
      && maybe True safeFilename _slSetName
  shrink    = genericShrink

instance Arbitrary Requirements where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary GHCup.Types.DownloadInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary GHCup.Types.Stack.DownloadInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary LinuxDistro where
  arbitrary =
    oneof (pure <$> allDistros)

instance Arbitrary Platform where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Tag where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Architecture where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary VersionInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary VersionRange where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary UserSettings where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary PagerConfig where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary PlatformRequest where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary MetaMode where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary GPGSetting where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Verbosity where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary URLSource where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary KeepDirs where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Downloader where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary SetupInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary UserKeyBindings where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary NewURLSource where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary ChannelAlias where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary KeyCombination where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Key where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Modifier where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary GHCDownloadInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary ProcessSpec where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary UserInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Authority where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary DownloadMirror where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary DownloadMirrors where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary VersionedDownloadInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary (NonEmpty VersionCmp) where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary VersionCmp where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary TarDir where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Tool where
  arbitrary = flip suchThat pred' $ do
    ASCIIString str <- arbitrary
    pure $ Tool (toLower <$> str)
   where
    pred' = safeToolname
  shrink    = genericShrink

instance Arbitrary GHCupInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary TargetVersion where
  arbitrary = suchThat (TargetVersion Nothing <$> arbitrary) pred'
   where
    pred' = safeVersion
  shrink    = genericShrink


-- our maps are nested... the default size easily blows up most ppls ram

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map Tool v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map (Maybe Version) v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map Platform v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (MapIgnoreUnknownKeys Platform v) where
  arbitrary = resize 8 $ MapIgnoreUnknownKeys . M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (MapIgnoreUnknownKeys Architecture v) where
  arbitrary = resize 8 $ MapIgnoreUnknownKeys . M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map (Maybe Versioning) v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary
