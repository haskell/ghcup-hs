{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

module GHCup.Utils.Pager where

import GHCup.Prelude
import System.Environment
import GHCup.Utils.Dirs (findExecutable)
import Control.Applicative (asum)
import System.Process
import System.Exit
import System.IO
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad (forM_)
import Control.Exception (IOException, try)


getPager :: IO (Maybe FilePath)
getPager = do
  lookupEnv "GHCUP_PAGER" >>= \case
    Just r  -> pure $ Just r
    Nothing -> lookupEnv "PAGER" >>= \case
      Just r' -> pure $ Just r'
      Nothing -> do
        let pagers
              | isWindows = ["most.exe", "more.exe", "less.exe"]
              | otherwise = ["most", "more", "less"]
        asum $ fmap findExecutable pagers

sendToPager :: FilePath -> [Text] -> IO (Either IOException ())
sendToPager pager text = do
  (Just stdinH, _, Just stderrH, ph) <-
    createProcess
      $ (shell pager) { std_in = CreatePipe
                      , std_err = CreatePipe
                      }
  try @IOException (forM_ text $ T.hPutStrLn stdinH) >>= \case
    Left e -> pure $ Left e
    Right _ -> do
      hClose stdinH
      exitCode <- waitForProcess ph
      case exitCode of
        ExitFailure i ->
          do errContents <- hGetContents stderrH
             fail (unlines [mappend "Pager exited with exit code " (show i)
                           ,errContents])
        _ -> pure $ Right ()

