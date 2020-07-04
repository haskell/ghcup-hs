{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | A module to retrieve os-release information according to the
-- freedesktop standard:
-- https://www.freedesktop.org/software/systemd/man/os-release.html
--
-- Usage example:
--
-- @
-- do
--   Just (OsRelease {..}) <- fmap osRelease <$\> parseOsRelease
--   putStrLn name
-- @
module System.OsRelease
  (
  -- * data types
    OsReleaseResult(..)
  , OsRelease(..)

  -- * read/parse os-release
  , parseOsRelease
  , readOsRelease

  -- * defaults
  , defaultOsRelease
  , defaultAssignments

  -- * low-level
  , parseAssignments
  , parseAssignment
  , getAllAssignments
  , getOsRelease
  , parseOsRelease'
  )
where

import           System.OsRelease.Megaparsec

import           Control.Applicative
import           Control.Monad
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Void
import           GHC.Generics
import           Prelude                 hiding ( id
                                                )

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP


data OsReleaseResult = OsReleaseResult {
    osRelease :: !OsRelease
  , unknown_fields :: [(String, String)]
  , parse_errors :: [MP.ParseError String Void]
} deriving (Show)


-- | All the explicitly documented fields of @os-release@.
data OsRelease = OsRelease {
    name :: !(String)
  , version :: !(Maybe String)
  , id :: !(String)
  , id_like :: !(Maybe String)
  , version_codename :: !(Maybe String)
  , version_id :: !(Maybe String)
  , pretty_name :: !(String)
  , ansi_color :: !(Maybe String)
  , cpe_name :: !(Maybe String)
  , home_url :: !(Maybe String)
  , documentation_url :: !(Maybe String)
  , support_url :: !(Maybe String)
  , bug_report_url :: !(Maybe String)
  , privacy_policy_url :: !(Maybe String)
  , build_id :: !(Maybe String)
  , variant :: !(Maybe String)
  , variant_id :: !(Maybe String)
  , logo :: !(Maybe String)
} deriving (Generic, Show)


class GetRecords a where
  getRecords :: a -> [String]

instance {-# OVERLAPPABLE #-} GetRecords (f p) => GetRecords (M1 i c f p) where
  getRecords (M1 x) = getRecords x

instance {-# OVERLAPPING #-} Selector c => GetRecords (M1 S c f p) where
  getRecords x = [selName x]

instance (GetRecords (a p), GetRecords (b p)) => GetRecords ((a :*: b) p) where
  getRecords (a :*: b) = getRecords a ++ getRecords b



-- | The defaults as per the spec:
--
-- @
-- NAME=Linux
-- ID=linux
-- PRETTY_NAME=Linux
-- @
defaultOsRelease :: OsRelease
defaultOsRelease = OsRelease { name               = "Linux"
                             , version            = Nothing
                             , id                 = "linux"
                             , id_like            = Nothing
                             , version_codename   = Nothing
                             , version_id         = Nothing
                             , pretty_name        = "Linux"
                             , ansi_color         = Nothing
                             , cpe_name           = Nothing
                             , home_url           = Nothing
                             , documentation_url  = Nothing
                             , support_url        = Nothing
                             , bug_report_url     = Nothing
                             , privacy_policy_url = Nothing
                             , build_id           = Nothing
                             , variant            = Nothing
                             , variant_id         = Nothing
                             , logo               = Nothing
                             }

-- | Like `defaultOsRelease`, except as key-value pair.
defaultAssignments :: [(String, String)]
defaultAssignments =
  [("NAME", "Linux"), ("ID", "linux"), ("PRETTY_NAME", "Linux")]


-- | Get all allAssignments as @(key, val)@ from the @os-release@
-- file contents.
getAllAssignments :: String  -- ^ file contents of os-release
                  -> [Either (MP.ParseError String Void) (String, String)]
getAllAssignments = fromRight [] . MP.parse parseAssignments "os-release"


-- | Parse the assignments into `OsRelease`. This is merged with the
-- defaults as per the spec. In case of no assignments, also returns
-- the defaults.
getOsRelease :: [(String, String)]  -- ^ assignments
             -> OsRelease
getOsRelease =
  (\case
      Error   _ -> defaultOsRelease
      Success v -> v
    )
    . fromJSON
    . Object
    . (\x -> HM.union x (HM.fromList . aesonify $ defaultAssignments))
    . HM.fromList
    . aesonify
 where
  aesonify = fmap (\(k, v) -> (T.toLower . T.pack $ k, String . T.pack $ v))


-- | Tries to read @\"\/etc\/os-release\"@ and @\"\/usr\/lib\/os_release\"@ in order.
--
-- Throws @IOError@ if both files could not be read.
readOsRelease :: IO String
readOsRelease = readFile "/etc/os-release" <|> readFile "/usr/lib/os-release"


-- | Tries to read @\"\/etc\/os-release\"@ and @\"\/usr\/lib\/os_release\"@ in order
-- and parses into `OsReleaseResult`. Returns @Nothing@ if both files could
-- not be read.
parseOsRelease :: IO (Maybe OsReleaseResult)
parseOsRelease =
  handleIO (\_ -> pure Nothing) . fmap (Just . parseOsRelease') $ readOsRelease


-- | Like `parseOsRelease`, except taking the input String explicitly.
-- Primarily for tests.
parseOsRelease' :: String -> OsReleaseResult
parseOsRelease' s =
  let (errs, ass) = partitionEithers . getAllAssignments $ s
      osr         = getOsRelease ass
      unknown_fields' =
          HM.toList
            . foldr (\x y -> HM.delete (fmap toUpper x) y) (HM.fromList ass)
            $ (init . getRecords . from $ defaultOsRelease)
  in  OsReleaseResult osr unknown_fields' errs


deriveJSON defaultOptions ''OsRelease
