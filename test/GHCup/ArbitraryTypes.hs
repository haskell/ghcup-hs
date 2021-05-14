{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.ArbitraryTypes where


import           GHCup.Types

import           Data.ByteString                ( ByteString )
import           Data.Versions
import           Data.List.NonEmpty
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
  arbitrary = oneof [ pure (Scheme "http"), pure (Scheme "https") ]

instance Arbitrary Host where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary Port where
  arbitrary = genericArbitrary
  shrink    = genericShrink

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

instance Arbitrary Requirements where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary DownloadInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary LinuxDistro where
  arbitrary = genericArbitrary
  shrink    = genericShrink

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
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary GHCupInfo where
  arbitrary = genericArbitrary
  shrink    = genericShrink


-- our maps are nested... the default size easily blows up most ppls ram

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map Tool v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map (Maybe Version) v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map Platform v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

instance {-# OVERLAPS #-} Arbitrary v => Arbitrary (M.Map (Maybe Versioning) v) where
  arbitrary = resize 8 $ M.fromList <$> arbitrary

