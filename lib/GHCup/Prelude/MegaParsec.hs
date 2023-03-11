{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-|
Module      : GHCup.Utils.MegaParsec
Description : MegaParsec utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Prelude.MegaParsec where

import           GHCup.Types

import           Control.Applicative
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Data.Functor
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Versions
import           Data.Void
import           System.FilePath

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import Data.Char (digitToInt)
import Data.Data (Proxy(..))


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
    <*> version'
 where
  verP' :: MP.Parsec Void Text Text
  verP' = do
    v <- version'
    let startsWithDigists =
          and
            . take 3
            . concatMap
              (map
                (\case
                  (Digits _) -> True
                  (Str    _) -> False
                ) . NE.toList)
            . NE.toList
            $ _vChunks v
    if startsWithDigists && isNothing (_vEpoch v)
      then pure $ prettyVer v
      else fail "Oh"

ghcTargetVerRevP :: MP.Parsec Void Text GHCTargetVersionRev
ghcTargetVerRevP =
  (\x y -> GHCTargetVersionRev x y)
    <$> (MP.try (Just <$> parseUntil1 (MP.chunk "-" *> verP') <* MP.chunk "-")
        <|> ((\ _ x -> x) Nothing <$> mempty)
        )
    <*> versionRevP
 where
  verP' :: MP.Parsec Void Text Text
  verP' = do
    v <- version'
    let startsWithDigists =
          and
            . take 3
            . concatMap
              (map
                (\case
                  (Digits _) -> True
                  (Str    _) -> False
                ) . NE.toList)
            . NE.toList
            $ _vChunks v
    if startsWithDigists && isNothing (_vEpoch v)
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

versionRevP :: MP.Parsec Void Text VersionRev
versionRevP = MP.label "versionRev" $
  MP.try (parseUntil (MP.try (MP.chunk "-r")) >>= versionWithRev) <|> ((`VersionRev` 0) <$> version')
 where
  versionWithRev ver = do
    rest <- MP.getInput
    MP.setInput ver
    v <- version'
    MP.setInput rest
    _ <- MP.chunk "-r"
    rev <- parseInt
    pure $ VersionRev v rev

  digit = MP.oneOf ['0'..'9'] MP.<?> "digit"
  parseInt :: MP.Parsec Void Text Int
  parseInt = MP.label "parseInt" $ do
    i <- MP.tokensToChunk (Proxy :: Proxy Text) <$> some digit
    pure $ numberValue 10 $ T.unpack i

  numberValue :: Int -> String -> Int
  numberValue base = foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0

userVersionRevP :: MP.Parsec Void Text UserVersionRev
userVersionRevP = MP.label "userVersionRev" $
  ((\(VersionRev v r) -> UserVersionRev v (Just r)) <$> MP.try versionRevP) <|> ((`UserVersionRev` Nothing) <$> version')


-- | Read a @VersionRev@ from a String.
--
-- - 3.3.2    -> VersionRev { vVersion = 3.3.3, vRev = 0 }
-- - 2.3.4-r3 -> VersionRev { vVersion = 2.3.4, vRev = 3 }
versionRev :: Text -> Either (MP.ParseErrorBundle Text Void) VersionRev
versionRev = MP.parse versionRevP ""

-- | Read a @UserVersionRev@ from a String.
--
-- - 3.3.2    -> UserVersionRev { vVersion = 3.3.3, vRev = Nothing }
-- - 2.3.4-r3 -> UserVersionRev { vVersion = 2.3.4, vRev = Just 3 }
userVersionRev :: Text -> Either (MP.ParseErrorBundle Text Void) UserVersionRev
userVersionRev = MP.parse userVersionRevP ""
