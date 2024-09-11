{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds             #-}

module GHCup.Prelude.File.Search (
  module GHCup.Prelude.File.Search
  , ProcessError(..)
  , CapturedProcess(..)
  ) where

import           GHCup.Prelude.Internal ((!?))
import           GHCup.Types(ProcessError(..), CapturedProcess(..))

import           Control.Monad (forM)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text               ( Text )
import           Data.Void
import           GHC.IO.Exception
import           System.Directory hiding ( removeDirectory
                                         , removeDirectoryRecursive
                                         , removePathForcibly
                                         , findFiles
                                         , makeAbsolute
                                         )
import           System.FilePath
import           Text.Regex.Posix


import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import Control.Exception.Safe (handleIO)
import System.Directory.Internal.Prelude (ioeGetErrorType)
import Data.Variant.Excepts (Excepts)
import GHCup.Errors (NotFoundInPATH(..))


makeAbsolute :: MonadIO m => FilePath -> Excepts '[NotFoundInPATH] m FilePath
makeAbsolute bin = do
  spaths <- liftIO getSearchPath
  liftIO (searchPath spaths bin) !? NotFoundInPATH bin


-- | Search for a file in the search paths.
--
-- Catches `PermissionDenied` and `NoSuchThing` and returns `Nothing`.
searchPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
searchPath paths needle = go paths
 where
  go [] = pure Nothing
  go (x : xs) =
    handleIO (\e -> if ioeGetErrorType e `elem` [InappropriateType, PermissionDenied, NoSuchThing] then go xs else ioError e)
      $ do
          contents <- listDirectory x
          findM (isMatch x) contents >>= \case
            Just _ -> pure $ Just (x </> needle)
            Nothing -> go xs
  isMatch basedir p = do
    if p == needle
      then isExecutable (basedir </> needle)
      else pure False

  isExecutable :: FilePath -> IO Bool
  isExecutable file = executable <$> getPermissions file

  -- TODO: inlined from GHCup.Prelude
  findM ~p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)
  ifM ~b ~t ~f = do
    b' <- b
    if b' then t else f


-- | Check wether a binary is shadowed by another one that comes before
-- it in PATH. Returns the path to said binary, if any.
isShadowed :: FilePath -> IO (Maybe FilePath)
isShadowed p = do
  let dir = takeDirectory p
  let fn = takeFileName p
  spaths <- liftIO getSearchPath
  if dir `elem` spaths
  then do
    let shadowPaths = takeWhile (/= dir) spaths
    searchPath shadowPaths fn
  else pure Nothing


-- | Check whether the binary is in PATH. This returns only `True`
-- if the directory containing the binary is part of PATH.
isInPath :: FilePath -> IO Bool
isInPath p = do
  let dir = takeDirectory p
  let fn = takeFileName p
  spaths <- liftIO getSearchPath
  if dir `elem` spaths
  then isJust <$> searchPath [dir] fn
  else pure False


-- | Follows the first match in case of Regex.
expandFilePath :: [Either FilePath Regex] -> IO [FilePath]
expandFilePath = go ""
 where
  go :: FilePath -> [Either FilePath Regex] -> IO [FilePath]
  go p [] = pure [p]
  go p (x:xs) = do
    case x of
      Left s -> go (p </> s) xs
      Right regex -> do
        fps <- findFiles p regex
        res <- forM fps $ \fp -> go (p </> fp) xs
        pure $ mconcat res


findFiles :: FilePath -> Regex -> IO [FilePath]
findFiles path regex = do
  contents <- listDirectory path
  pure $ filter (match regex) contents


findFiles' :: FilePath -> MP.Parsec Void Text a -> IO [FilePath]
findFiles' path parser = do
  contents <- listDirectory path
  pure $ filter (\fp -> either (const False) (const True) $ MP.parse parser "" (T.pack fp)) contents


