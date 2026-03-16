{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Prelude.MegaParsec
Description : MegaParsec utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Prelude.MegaParsec where

import GHCup.Types

import Control.Applicative
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Data.Functor
import Data.Maybe
import Data.Text       ( Text )
import Data.Versions
import Data.Void
import System.FilePath

import           Data.List.NonEmpty   ( NonEmpty ((:|)) )
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MPC


choice' :: (MonadFail f, MP.MonadParsec e s f) => [f a] -> f a
choice' []       = fail "Empty list"
choice' [x     ] = x
choice' (x : xs) = MP.try x <|> choice' xs


parseUntil :: MP.Parsec Void Text a -> MP.Parsec Void Text Text
parseUntil p = do
  (MP.try (MP.lookAhead p) $> mempty)
    <|> (do
          c  <- T.singleton <$> MP.anySingle
          c2 <- parseUntil p
          pure (c `mappend` c2)
        )

parseUntil1 :: MP.Parsec Void Text a -> MP.Parsec Void Text Text
parseUntil1 p = do
  i1 <- MP.getOffset
  t <- parseUntil p
  i2 <- MP.getOffset
  if i1 == i2 then fail "empty parse" else pure t



-- | Parses e.g.
--   * armv7-unknown-linux-gnueabihf-ghc
--   * armv7-unknown-linux-gnueabihf-ghci
ghcTargetBinP :: Text -> MP.Parsec Void Text (Maybe Text, Text)
ghcTargetBinP t =
  (,)
    <$> (   MP.try
            (Just <$> parseUntil1 (MP.chunk "-" *> MP.chunk t) <* MP.chunk "-"
            )
        <|> ((\ _ x -> x) Nothing <$> mempty)
        )
    <*> (MP.chunk t <* MP.eof)


-- | Extracts the version from @ProjectVersion="8.10.5"@.
ghcProjectVersion :: MP.Parsec Void Text Version
ghcProjectVersion = do
  _ <- MP.chunk "ProjectVersion=\""
  ver <- parseUntil1 $ MP.chunk "\""
  MP.setInput ver
  version'


-- | Extracts target triple and version from e.g.
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * armv7-unknown-linux-gnueabihf-8.8.3
ghcTargetVerP :: MP.Parsec Void Text GHCTargetVersion
ghcTargetVerP =
  (\x y -> GHCTargetVersion x y)
    <$> (MP.try (Just <$> parseUntil1 (MP.chunk "-" *> verP') <* MP.chunk "-")
        <|> ((\ _ x -> x) Nothing <$> mempty)
        )
    <*> (version' <* MP.eof)
 where
  verP' :: MP.Parsec Void Text Text
  verP' = do
    v <- version'
    let startsWithDigits =
          and
            . take 3
            . map (\case
                      Numeric  _ -> True
                      Alphanum _ -> False)
            . NE.toList
            . (\(Chunks nec) -> nec)
            $ _vChunks v
    if startsWithDigits && isNothing (_vEpoch v)
      then pure $ prettyVer v
      else fail "Oh"

ghcLinkVersion :: MP.Parsec Void Text GHCTargetVersion
ghcLinkVersion =
  (\x y -> GHCTargetVersion x y)
    <$>
       (MP.try (Just <$> parseUntil1 (MP.chunk "-ghc-" *> verP') <* MP.chunk "-")
        <|> ((\ _ x -> x) Nothing <$> mempty)
        )
    <*> (MP.chunk "ghc-" *> version' <* MP.eof)
 where
  verP' :: MP.Parsec Void Text Text
  verP' = do
    v <- version'
    let startsWithDigits =
          and
            . take 3
            . map (\case
                      Numeric  _ -> True
                      Alphanum _ -> False)
            . NE.toList
            . (\(Chunks nec) -> nec)
            $ _vChunks v
    if startsWithDigits && isNothing (_vEpoch v)
      then pure $ prettyVer v
      else fail "Oh"


verP :: MP.Parsec Void Text Text -> MP.Parsec Void Text Versioning
verP suffix = do
  ver <- parseUntil suffix
  if T.null ver
    then fail "empty version"
    else do
      rest <- MP.getInput
      MP.setInput ver
      v <- versioning'
      MP.setInput rest
      pure v


pathSep :: MP.Parsec Void Text Char
pathSep = MP.oneOf pathSeparators

skipWhile :: (Char -> Bool) -> MP.Parsec Void Text ()
skipWhile f = void $ MP.takeWhileP Nothing f

skip :: (Char -> Bool) -> MP.Parsec Void Text ()
skip f = void $ MP.satisfy f

skipSpace :: MP.Parsec Void Text ()
skipSpace = void $ MP.satisfy isSpace

skipSpaces :: MP.Parsec Void Text ()
skipSpaces = void $ many skipSpace

isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- Obtain the version from the link or shim path
-- ../ghc/<ver>/bin/ghc
-- ../ghc/<ver>/bin/ghc-<ver>
ghcVersionFromPath :: MP.Parsec Void Text GHCTargetVersion
ghcVersionFromPath =
  do
     beforeBin <- parseUntil1 binDir <* MP.some pathSep
     MP.setInput beforeBin
     _ <- parseTillLastPathSep
     ghcTargetVerP
  where
     binDir = MP.some pathSep <* MP.chunk "bin" *> MP.some pathSep <* MP.takeWhile1P Nothing (not . isPathSeparator) <* MP.eof
     parseTillLastPathSep = (MP.try (parseUntil1 pathSep *> MP.some pathSep) *> parseTillLastPathSep) <|> pure ()

versionCmpP :: MP.Parsec Void T.Text VersionCmp
versionCmpP = either (fail . T.unpack) pure =<< (translate <$> (MPC.space *> MP.try (MP.takeWhileP Nothing (`elem` ['>', '<', '=']))) <*> (MPC.space *> versioningEnd))
 where
   translate ">" v  = Right $ VR_gt v
   translate ">=" v = Right $ VR_gteq v
   translate "<" v  = Right $ VR_lt v
   translate "<=" v = Right $ VR_lteq v
   translate "==" v = Right $ VR_eq v
   translate "" v   = Right $ VR_eq v
   translate c  _   = Left $ "unexpected comparator: " <> c

versionRangeP :: MP.Parsec Void T.Text VersionRange
versionRangeP = go <* MP.eof
 where
  go =
    MP.try orParse
      <|> MP.try (fmap SimpleRange andParse)
      <|> fmap (SimpleRange . pure) versionCmpP

  orParse :: MP.Parsec Void T.Text VersionRange
  orParse =
    (\a o -> OrRange a o)
      <$> (MP.try andParse <|> fmap pure versionCmpP)
      <*> (MPC.space *> MP.chunk "||" *> MPC.space *> go)

  andParse :: MP.Parsec Void T.Text (NonEmpty VersionCmp)
  andParse =
    fmap (\h t -> h :| t)
         (MPC.space *> MP.chunk "(" *> MPC.space *> versionCmpP)
      <*> MP.try (MP.many (MPC.space *> MP.chunk "&&" *> MPC.space *> versionCmpP))
      <*  MPC.space
      <*  MP.chunk ")"
      <*  MPC.space

versioningEnd :: MP.Parsec Void T.Text Versioning
versioningEnd =
  MP.try (verP (MP.chunk " " <|> MP.chunk ")" <|> MP.chunk "&&") <* MPC.space)
    <|> versioning'

