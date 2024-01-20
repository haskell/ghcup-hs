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
import Control.Monad.Catch (throwM)
import GHCup.Errors (ParseError(..))
import Text.Megaparsec
import Data.Void (Void)

-- | This reflects the API version of the YAML.
--
-- Note that when updating this, CI requires that the file exists AND the same file exists at
-- 'https://www.haskell.org/ghcup/exp/ghcup-<ver>.yaml' with some newlines added.
ghcupURL :: URI
ghcupURL = [uri|https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.0.8.yaml|]

stackSetupURL :: URI
stackSetupURL = [uri|https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml|]

shimGenURL :: URI
shimGenURL = [uri|https://downloads.haskell.org/~ghcup/shimgen/shim-2.exe|]

shimGenSHA :: T.Text
shimGenSHA = T.pack "7c55e201f71860c5babea886007c8fa44b861abf50d1c07e5677eb0bda387a70"

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
versionToPVP v = case parse pvp'' "Version->PVP" $ V.prettyVer v of
  Left _  -> throwM $ ParseError "Couldn't convert Version to PVP"
  Right r -> pure r
 where
   pvp'' :: Parsec Void T.Text (V.PVP, T.Text)
   pvp'' = do
     p <- V.pvp'
     s <- getParserState
     pure (p, stateInput s)

pvpFromList :: [Int] -> V.PVP
pvpFromList = V.PVP . NE.fromList . fmap fromIntegral
