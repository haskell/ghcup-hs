{-|
Module      : GHCup.Prelude.Version
Description : Manipulating versions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Prelude.Version where

import GHCup.Types

import Control.Exception.Safe ( MonadThrow )
import Control.Monad.Catch    ( throwM )
import Data.List.NonEmpty     ( NonEmpty ((:|)) )
import Data.Text              ( Text )
import Data.Void              ( Void )
import GHCup.Errors           ( ParseError (..) )
import Text.Megaparsec

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Data.Versions      as V
import Control.Exception (Exception(displayException))


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

-- | Extract (major, minor) from any version.
getMajorMinorV :: MonadThrow m => V.Version -> m (Int, Int)
getMajorMinorV (V.Version _ (V.Chunks (V.Numeric x :| V.Numeric y : _)) _ _) = pure (fromIntegral x, fromIntegral y)
getMajorMinorV _ = throwM $ ParseError "Could not parse X.Y from version"

matchMajor :: V.Version -> Int -> Int -> Bool
matchMajor v' major' minor' = case getMajorMinorV v' of
  Just (x, y) -> x == major' && y == minor'
  Nothing     -> False

-- | Match PVP prefix.
--
-- >>> matchPVPrefix [pver|8.8|] [pver|8.8.4|]
-- True
-- >>> matchPVPrefix [pver|8|] [pver|8.8.4|]
-- True
-- >>> matchPVPrefix [pver|8.10|] [pver|8.8.4|]
-- False
-- >>> matchPVPrefix [pver|8.10|] [pver|8.10.7|]
-- True
matchPVPrefix :: V.PVP -> V.PVP -> Bool
matchPVPrefix (toL -> prefix) (toL -> full) = and $ zipWith (==) prefix full

toL :: V.PVP -> [Int]
toL (V.PVP inner) = fmap fromIntegral $ NE.toList inner

-- | Expand a list of version patterns describing a string such as "%v-%h".
--
-- >>> expandVersionPattern (either (const Nothing) Just $ version "3.4.3") "a386748" "a3867484ccc391daad1a42002c3a2ba6a93c5221" "v0.1.20.0-119-ga386748" "issue-998" [CabalVer, S "-", GitHashShort, S "-", GitHashLong, S "-", GitBranchName, S "-", GitDescribe, S "-coco"]
-- Version {_vEpoch = Nothing, _vChunks = Chunks (Numeric 3 :| [Numeric 4,Numeric 3]), _vRel = Just (Release (Alphanum "a386748-a3867484ccc391daad1a42002c3a2ba6a93c5221-issue-998-v0" :| [Numeric 1,Numeric 20,Alphanum "0-119-ga386748-coco"])), _vMeta = Nothing}
expandVersionPattern :: MonadFail m
                     => Maybe V.Version  -- ^ cabal ver
                     -> String         -- ^ git hash (short), if any
                     -> String         -- ^ git hash (long), if any
                     -> String         -- ^ git describe output, if any
                     -> String         -- ^ git branch name, if any
                     -> [VersionPattern]
                     -> m V.Version
expandVersionPattern cabalVer gitHashS gitHashL gitDescribe gitBranch
  = either (fail . displayException) pure . V.version . T.pack . go
 where
  go []                 = ""
  go (CabalVer:xs)      = T.unpack (maybe T.empty V.prettyVer cabalVer) <> go xs
  go (GitHashShort:xs)  = gitHashS <> go xs
  go (GitHashLong:xs)   = gitHashL <> go xs
  go (GitDescribe:xs)   = gitDescribe <> go xs
  go (GitBranchName:xs) = gitBranch <> go xs
  go (S str:xs)         = str <> go xs

