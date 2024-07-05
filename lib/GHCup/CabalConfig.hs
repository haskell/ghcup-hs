{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module GHCup.CabalConfig (getStoreDir) where

import Data.ByteString          (ByteString)
import Data.List.NonEmpty       (NonEmpty)
import Data.Map                 (Map)
import System.Directory         (getAppUserDataDirectory, doesDirectoryExist, getXdgDirectory, XdgDirectory(XdgConfig))
import System.Environment       (lookupEnv)
import System.FilePath          ((</>))

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as M
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.FieldGrammar     as C
import qualified Distribution.FieldGrammar.Parsec     as C
import qualified Distribution.Fields           as C
import qualified Distribution.Fields.LexerMonad as C
import qualified Distribution.Parsec           as C
import qualified Distribution.Utils.Generic    as C
import qualified Text.Parsec                    as P

import Data.Foldable              (for_)
import Distribution.Parsec.Error




getStoreDir :: IO FilePath
getStoreDir = do
    fp <- findConfig
    bs <- BS.readFile fp
    either (fail . show . fmap (showPError fp)) resolveConfig (parseConfig bs)

-------------------------------------------------------------------------------
-- Find config
-------------------------------------------------------------------------------

-- | Find the @~\/.cabal\/config@ file.
findConfig :: IO FilePath
findConfig = do
    env <- lookupEnv "CABAL_CONFIG"
    case env of
        Just p -> return p
        Nothing -> do
            cabalDir <- findCabalDir
            return (cabalDir </> "config")

-- | Find the @~\/.cabal@ dir.
findCabalDir :: IO FilePath
findCabalDir = do
    cabalDirVar <- lookupEnv "CABAL_DIR"
    appDir <- getAppUserDataDirectory "cabal"
    isXdg <- not <$> doesDirectoryExist appDir
    if | Just dir <- cabalDirVar -> pure dir
       | isXdg -> getXdgDirectory XdgConfig "cabal"
       | otherwise -> pure appDir


-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse @~\/.cabal\/config@ file.
parseConfig :: ByteString -> Either (NonEmpty PError) (Maybe FilePath)
parseConfig = parseWith $ \fields0 -> do
    let (fields1, _) = C.partitionFields fields0
    let fields2 = M.filterWithKey (\k _ -> k `elem` knownFields) fields1
    parse fields2
  where
    knownFields = C.fieldGrammarKnownFieldList grammar

    parse :: Map C.FieldName [C.NamelessField C.Position]
          -> C.ParseResult (Maybe FilePath)
    parse fields = C.parseFieldGrammar C.cabalSpecLatest fields grammar

grammar :: C.ParsecFieldGrammar (Maybe FilePath) (Maybe FilePath)
grammar = mempty
    <$> C.optionalFieldAla "store-dir" C.FilePathNT id

parseWith
    :: ([C.Field C.Position] -> C.ParseResult a)  -- ^ parse
    -> ByteString                                 -- ^ contents
    -> Either (NonEmpty PError) a
parseWith parser bs = case C.runParseResult result of
    (_, Right x)      -> Right x
    (_, Left (_, es)) -> Left es
  where
    result = case C.readFields' bs of
        Left perr -> C.parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = C.Position (P.sourceLine ppos) (P.sourceColumn ppos)
        Right (fields, lexWarnings) -> do
            C.parseWarnings (C.toPWarnings lexWarnings)
            for_ (C.validateUTF8 bs) $ \pos ->
                C.parseWarning C.zeroPos C.PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
            parser fields

-------------------------------------------------------------------------------
-- Resolving
-------------------------------------------------------------------------------

-- | Fill the default in @~\/.cabal\/config@  file.
resolveConfig :: Maybe FilePath -> IO FilePath
resolveConfig (Just fp) = pure fp
resolveConfig Nothing = do
    c <- findCabalDir
    return (c </> "store")

