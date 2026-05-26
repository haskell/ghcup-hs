{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : GHCup.Input.Parsers.Domain
Description : Domain parsers and bashism features for the metadata
Copyright   : (c) Julian Ospald, 2026
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Input.Parsers.Domain where

import GHCup.Types
import GHCup.Errors

import Data.Bifunctor (bimap)
import Data.Versions                ( Version, version )
import Control.Applicative ( Alternative ((<|>), many) )
import Data.Maybe
import Data.Void
import Text.Regex.Posix
import Data.Variant.Excepts

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Text.Megaparsec as MP


evalDomainVal :: M.Map DomainVariable String -> (DomainVariable, Maybe Substitution) -> Maybe String
evalDomainVal env (dvar, msubst) = do
  var <- M.lookup dvar env
  pure $ maybe var (substitute var) msubst
 where
  mkRegex = makeRegexOpts compBlank execBlank
  stripSuffix (T.pack -> suff) (T.pack -> t) = fmap T.unpack $ T.stripSuffix suff t
  stripPrefix (T.pack -> prev) (T.pack -> t) = fmap T.unpack $ T.stripPrefix prev t
  breakOn (T.pack -> needle) (T.pack -> haystack) = bimap T.unpack T.unpack $ T.breakOn needle haystack
  replace (T.pack -> needle) (T.pack -> replacement) (T.pack -> haystack) = T.unpack $ T.replace needle replacement haystack

  substitute var (Replace needle replacement)
    = let matchedNeedle = match (mkRegex (globToRegex needle)) var
          (pre, rest) = breakOn matchedNeedle var
      in case stripPrefix matchedNeedle rest of
           Nothing    -> var
           Just rest' -> pre <> replacement <> rest'
  substitute var (ReplaceAll needle replacement)
    = let matchedNeedle = match (mkRegex (globToRegex needle)) var
      in replace matchedNeedle replacement var
  substitute var (StripPrefix prefix)
    = let matchedPrefix = match (mkRegex ("^" <> globToRegex prefix)) var
      in fromMaybe var (stripPrefix matchedPrefix var)
  substitute var (StripSuffix suffix)
    = let matchedSuffix = match (mkRegex (globToRegex suffix <> "$")) var
      in fromMaybe var (stripSuffix matchedSuffix var)

-- https://www.man7.org/linux/man-pages/man7/glob.7.html
globToRegex :: String -> String
globToRegex = go
 where
  go :: String -> String
  go ('\\':'*':xs) = '\\' : '*' : go xs
  go ('\\':'?':xs) =        '?' : go xs
  go ('*':xs) = '.'  : '*' : go xs
  go ('?':xs) = '.'                  : go xs
  go ('[':'!':']':xs) = '\\' : '[' : '!' : ']' : go xs
  go ('[':'!':xs) = '[' : '^'        : charClass xs
  go ('[':']':xs) = '[' : '\\' : ']' : charClass xs
  go ('[':xs)     = '['              : charClass xs
  go (x:xs)
    | x `elem` specialChars = '\\' : x : go xs
    | otherwise             =        x : go xs
  go []       = []

  charClass :: String -> String
  charClass ('\\':']':xs) = '\\' : ']' : charClass xs
  charClass (']':xs)      = ']'        : go xs
  charClass (x:xs)
    | x `elem` specialChars = '\\' : x : charClass xs
    | otherwise             =        x : charClass xs
  charClass  [] = []

  -- https://stackoverflow.com/a/400316
  specialChars :: String
  specialChars = ".^$*[\\"

domainParserP :: MP.Parsec Void String (DomainVariable, Maybe Substitution)
domainParserP = (,) <$> (MP.chunk "${" *> var) <*> MP.optional (MP.try subst) <* MP.chunk "}"
 where
  subst = MP.try replaceP <|> MP.try replaceAllP <|> MP.try stripSuffixP <|> MP.try stripPrefixP
  replaceP = Replace <$> (MP.chunk "/" *> MP.some escaped <* MP.chunk "/") <*> MP.many escaped
  replaceAllP = ReplaceAll <$> (MP.chunk "//" *> MP.some escaped <* MP.chunk "/") <*> MP.many escaped
  stripPrefixP = StripPrefix <$> (MP.chunk "#" *> MP.some escaped)
  stripSuffixP = StripSuffix <$> (MP.chunk "%" *> MP.some escaped)
  escaped :: MP.Parsec Void String Char
  escaped = do
    t <- MP.lookAhead MP.anySingle
    if | t == '\\'
       -> MP.chunk "\\" *> MP.anySingle
       | otherwise
       -> MP.noneOf ("/%#}{$" :: String)

  var =   (TARGETFN <$ MP.try (MP.chunk "TARGETFN"))
      <|> (PKGVER   <$ MP.try (MP.chunk "PKGVER"))
      <|> (PREFIX   <$ MP.try (MP.chunk "PREFIX"))
      <|> (TMPDIR   <$ MP.try (MP.chunk "TMPDIR"))



throwOnParseError :: (Monad m, Show a) => Either a b -> Excepts '[ParseError] m b
throwOnParseError = either (throwE . ParseError . show) pure

domainParser :: M.Map DomainVariable String -> MP.Parsec Void String String
domainParser env = domainParser' env []

domainParser' :: M.Map DomainVariable String -> [DomainVariable] -> MP.Parsec Void String String
domainParser' env skip = concat <$> many anyOrKnownVars
 where
  anyOrKnownVars :: MP.Parsec Void String String
  anyOrKnownVars = do
      (MP.try (Just <$> MP.lookAhead ((,,) <$> MP.anySingle <*> MP.anySingle <*> MP.anySingle)) <|> pure Nothing) >>= \case
        Just ('\\', '$', '{') -> do
          _ <- MP.anySingle
          fmap (:[]) MP.anySingle
        Just ('$', '{', _) -> do
          (matched, r@(dvar, _)) <- MP.match domainParserP
          case evalDomainVal env r of
            Just x -> pure x
            Nothing
              | dvar `elem` skip -> pure matched
              | otherwise -> fail $ "Variable " <> show dvar <> " not found in env"
        _ -> fmap (:[]) MP.anySingle

domainParserVer :: M.Map DomainVariable String -> MP.Parsec Void String [Either Char Version]
domainParserVer env = many anyOrKnownVars
 where
  anyOrKnownVars :: MP.Parsec Void String (Either Char Version)
  anyOrKnownVars = do
      (MP.try (Just <$> MP.lookAhead ((,,) <$> MP.anySingle <*> MP.anySingle <*> MP.anySingle)) <|> pure Nothing) >>= \case
        Just ('\\', '$', '{') -> do
          _ <- MP.anySingle
          fmap Left MP.anySingle
        Just ('$', '{', _) -> do
          r <- domainParserP
          v <- maybe (fail "Variable not found in env") pure (evalDomainVal env r)
          v' <- either (fail . show) pure $ version (T.pack v)
          pure (Right v')
        _ -> fmap Left MP.anySingle
