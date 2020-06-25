{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}

-- | A module to retrieve os-release information according to the
-- freedesktop standard:
-- https://www.freedesktop.org/software/systemd/man/os-release.html
--
-- Some of it is stolen from:
-- https://hackage.haskell.org/package/os-release-0.2.2/docs/src/System-OsRelease.html
module GHCup.Utils.OsRelease where

import           GHCup.Utils.MegaParsec

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Void
import           HPath
import           HPath.IO
import           Prelude                 hiding ( abs
                                                , readFile
                                                )

import qualified Data.ByteString.Lazy.UTF8     as UTF8
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP


-- | All the explicitly documented fields of `os-release`.
data OsRelease = OsRelease {
    name :: Maybe String
  , version :: Maybe String
  , id :: Maybe String
  , id_like :: Maybe String
  , version_codename :: Maybe String
  , version_id :: Maybe String
  , pretty_name :: Maybe String
  , ansi_color :: Maybe String
  , cpe_name :: Maybe String
  , home_url :: Maybe String
  , documentation_url :: Maybe String
  , support_url :: Maybe String
  , bug_report_url :: Maybe String
  , privacy_policy_url :: Maybe String
  , build_id :: Maybe String
  , variant :: Maybe String
  , variant_id :: Maybe String
  , logo :: Maybe String
} deriving (Show)

emptyOsRelease :: OsRelease
emptyOsRelease = OsRelease { name               = Nothing
                           , version            = Nothing
                           , id                 = Nothing
                           , id_like            = Nothing
                           , version_codename   = Nothing
                           , version_id         = Nothing
                           , pretty_name        = Nothing
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

-- | Parse a single line assignment and extract the right hand side.
-- This is only a subset of a shell parser, see
-- https://www.freedesktop.org/software/systemd/man/os-release.html
parseAssignment :: MP.Parsec Void String (String, String)
parseAssignment =
  (,)
    <$> (MP.space *> key)
    <*> (MP.char '=' *> (MP.try qval <|> mempty) <* MP.space <* MP.eof)
 where
  dropSpace :: String -> String
  dropSpace = reverse . dropWhile (\x -> x == ' ' || x == '\t') . reverse

  key :: MP.Parsec Void String String
  key = some (MP.try MP.alphaNumChar <|> MP.char '_')

  qval :: MP.Parsec Void String String
  qval = do
    c <- MP.lookAhead MP.printChar
    case c of
      ' '  -> pure ""
      '"'  -> MP.char c *> val c <* MP.char c
      '\'' -> MP.char c *> val c <* MP.char c
      -- no quote, have to drop trailing spaces
      _    -> fmap dropSpace (some MP.alphaNumChar)
  val :: Char -> MP.Parsec Void String String
  val q = many (qspecial q <|> MP.noneOf (specials q)) -- noneOf may be too lax

  qspecial :: Char -> MP.Parsec Void String Char
  qspecial q =
    fmap (!! 1)
      . choice'
      . fmap (\s -> MP.try . MP.chunk $ ['\\', s])
      $ (specials q)

  specials :: Char -> [Char]
  specials q = [q, '\\', '$', '`']


-- | Get all allAssignments as `(key, val)` from the `os-release`
-- file contents.
allAssignments :: String  -- ^ file contents of os-release
               -> [(String, String)]
allAssignments = rights . fmap (MP.parse parseAssignment "") . lines


-- | Parse the assignments into OsRelease.
--
-- This can't fail and will create an "empty" product type instead on
-- failure.
osRelease :: [(String, String)]  -- ^ assignments
          -> OsRelease
osRelease =
  (\case
      Error   _ -> emptyOsRelease
      Success v -> v
    )
    . fromJSON
    . Object
    . HM.fromList
    . fmap (\(k, v) -> (T.toLower . T.pack $ k, String . T.pack $ v))


-- | Tries to read `/etc/os-release` and `/usr/lib/os_release` in order.
-- Throws an exception if both files do not exist.
readOsRelease :: IO String
readOsRelease = do
  let os_release1 :: Path Abs
      os_release1 = [abs|/etc/os-release|]
  let os_release2 :: Path Abs
      os_release2 = [abs|/usr/lib/os-release|]

  bs <- readFile os_release1 <|> readFile os_release2
  -- os-release is utf8
  pure . UTF8.toString $ bs


-- | Tries to read `/etc/os-release` and `/usr/lib/os_release` in order
-- and parses into `OsRelease`. Throws an exception if both files do not
-- exist.
parseOsRelease :: IO OsRelease
parseOsRelease = fmap (osRelease . allAssignments) readOsRelease


deriveJSON defaultOptions ''OsRelease
