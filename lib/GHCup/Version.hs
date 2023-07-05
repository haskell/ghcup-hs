{-# LANGUAGE QuasiQuotes       #-}


{-|
Module      : GHCup.Version
Description : Version information and version handling.
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Version where

import           GHCup.Types
import           Paths_ghcup (version)

import           Data.Version (Version(versionBranch))
import           URI.ByteString
import           URI.ByteString.QQ

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Data.Versions as V
import Control.Exception.Safe (MonadThrow)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (intersperse)
import Control.Monad.Catch (throwM)
import GHCup.Errors (ParseError(..))

-- | This reflects the API version of the YAML.
--
-- Note that when updating this, CI requires that the file exsists AND the same file exists at
-- 'https://www.haskell.org/ghcup/exp/ghcup-<ver>.yaml' with some newlines added.
ghcupURL :: URI
ghcupURL = [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.0.7.yaml|]

-- | The current ghcup version.
ghcUpVer :: V.PVP
ghcUpVer = V.PVP . NE.fromList . fmap fromIntegral $ versionBranch version

-- | ghcup version as numeric string.
numericVer :: String
numericVer = T.unpack . V.prettyPVP $ ghcUpVer

versionCmp :: V.Versioning -> VersionCmp -> Bool
versionCmp ver1 (VR_gt ver2)   = ver1 > ver2
versionCmp ver1 (VR_gteq ver2) = ver1 >= ver2
versionCmp ver1 (VR_lt ver2)   = ver1 < ver2
versionCmp ver1 (VR_lteq ver2) = ver1 <= ver2
versionCmp ver1 (VR_eq ver2)   = ver1 == ver2

versionRange :: V.Versioning -> VersionRange -> Bool
versionRange ver' (SimpleRange cmps) = all (versionCmp ver') cmps
versionRange ver' (OrRange cmps range) = 
  versionRange ver' (SimpleRange cmps) || versionRange ver' range

pvpToVersion :: MonadThrow m => V.PVP -> Text -> m V.Version
pvpToVersion pvp_ rest =
  either (\_ -> throwM $ ParseError "Couldn't convert PVP to Version") pure . V.version . (<> rest) . V.prettyPVP $ pvp_

-- | Convert a version to a PVP and unparsable rest.
--
-- -- prop> \v -> let (Just (pvp', r)) = versionToPVP v in pvpToVersion pvp' r === Just v
versionToPVP :: MonadThrow m => V.Version -> m (V.PVP, Text)
versionToPVP (V.Version (Just _) _ _ _) = throwM $ ParseError "Unexpected epoch"
versionToPVP v = either (\_ -> (, rest v) <$> alternative v) (pure . (, mempty)) . V.pvp . V.prettyVer $ v
 where
  alternative :: MonadThrow m => V.Version -> m V.PVP
  alternative v' = case NE.takeWhile isDigit (V._vChunks v') of
    [] -> throwM $ ParseError "Couldn't convert Version to PVP"
    xs -> pure $ pvpFromList (unsafeDigit <$> xs)

  rest :: V.Version -> Text
  rest (V.Version _ cs pr me) =
    let chunks = NE.dropWhile isDigit cs
        ver = intersperse (T.pack ".") . chunksAsT $ chunks
        me' = maybe [] (\m -> [T.pack "+",m]) me
        pr' = foldable [] (T.pack "-" :) $ intersperse (T.pack ".") (chunksAsT pr)
        prefix = case (ver, pr', me') of
                   (_:_, _, _) -> T.pack "."
                   _           -> T.pack ""
    in prefix <> mconcat (ver <> pr' <> me')
   where
    chunksAsT :: Functor t => t V.VChunk -> t Text
    chunksAsT = fmap (foldMap f)
      where
        f :: V.VUnit -> Text
        f (V.Digits i) = T.pack $ show i
        f (V.Str s)    = s

    foldable :: Foldable f => f b -> (f a -> f b) -> f a -> f b
    foldable d g f | null f    = d
                   | otherwise = g f



  isDigit :: V.VChunk -> Bool
  isDigit (V.Digits _ :| []) = True
  isDigit _                = False

  unsafeDigit :: V.VChunk -> Int
  unsafeDigit (V.Digits x :| []) = fromIntegral x
  unsafeDigit _ = error "unsafeDigit: wrong input"

pvpFromList :: [Int] -> V.PVP
pvpFromList = V.PVP . NE.fromList . fmap fromIntegral
