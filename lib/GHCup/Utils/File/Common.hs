{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module GHCup.Utils.File.Common where

import           GHCup.Utils.Prelude

import           Control.Monad.Extra
import           Control.Monad.Reader
import           Data.Maybe
import           GHC.IO.Exception
import           Optics                  hiding ((<|), (|>))
import           System.Directory
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass hiding ( (<>) )
import           Text.Regex.Posix

import qualified Data.ByteString.Lazy          as BL



data ProcessError = NonZeroExit Int FilePath [String]
                  | PTerminated FilePath [String]
                  | PStopped FilePath [String]
                  | NoSuchPid FilePath [String]
                  deriving Show

instance Pretty ProcessError where
  pPrint (NonZeroExit e exe args) =
    text "Process " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text " failed with exit code " <+> text (show e) <+> "."
  pPrint (PTerminated exe args) =
    text "Process " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text " terminated."
  pPrint (PStopped exe args) =
    text "Process " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text " stopped."
  pPrint (NoSuchPid exe args) =
    text "Could not find PID for process running " <+> pPrint exe <+> text " with arguments " <+> text (show args) <+> text "."

data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut   :: BL.ByteString
  , _stdErr   :: BL.ByteString
  }
  deriving (Eq, Show)

makeLenses ''CapturedProcess



-- | Search for a file in the search paths.
--
-- Catches `PermissionDenied` and `NoSuchThing` and returns `Nothing`.
searchPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
searchPath paths needle = go paths
 where
  go [] = pure Nothing
  go (x : xs) =
    hideErrorDefM [InappropriateType, PermissionDenied, NoSuchThing] (go xs)
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


findFiles :: FilePath -> Regex -> IO [FilePath]
findFiles path regex = do
  contents <- listDirectory path
  pure $ filter (match regex) contents


checkFileAlreadyExists :: (MonadIO m) => FilePath -> m Bool
checkFileAlreadyExists fp = liftIO $ doesFileExist fp
