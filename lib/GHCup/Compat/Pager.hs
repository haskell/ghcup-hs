{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.Compat.Pager where

import GHCup.Compat.Terminal
import GHCup.System.Directory

import Control.Exception  ( IOException, try )
import Control.Monad      ( forM_, (<=<) )
import Data.Foldable      ( asum )
import Data.Text          ( Text )
import System.Environment
import System.Exit
import System.IO
import System.Process

import qualified Data.Text    as T
import qualified Data.Text.IO as T


getPager :: IO (Maybe FilePath)
getPager = do
  lookupEnv "GHCUP_PAGER" >>= \case
    Just r  -> pure $ Just r
    Nothing -> lookupEnv "PAGER" >>= \case
      Just r' -> pure $ Just r'
      Nothing ->
        let pagers = ["most", "more", "less"]
        in fmap (either (const Nothing) Just)
           . try @IOException
           . asum
           . fmap (maybe (fail "could not find") pure <=< findExecutable)
           $ pagers

-- 'more' reads from STDERR, and requires std_err to be 'Inherit'
sendToPager :: FilePath -> [Text] -> IO (Either IOException ())
sendToPager pager text = try @IOException
    $ withCreateProcess (shell pager) { std_in = CreatePipe
                                      , std_err = Inherit
                                      , std_out = Inherit
                                      , delegate_ctlc = True
                                      }
    $ \mStdinH _ _ ph -> case mStdinH of
          Just stdinH -> do
            forM_ text $ T.hPutStrLn stdinH
            hClose stdinH
            exitCode <- waitForProcess ph
            case exitCode of
              ExitFailure i -> fail ("Pager exited with exit code " <> show i)
              _             -> pure ()
          Nothing -> fail "well, I got nothing!"


sendToPager' :: Maybe FilePath -> [Text] -> IO ()
sendToPager' (Just pager) text = do
  fits <- fitsInTerminal text
  case fits of
    Just True -> do
      T.putStr $ T.unlines text
    _ -> sendToPager pager text >>= \case
      Right _ -> pure ()
      Left _ -> do
        T.putStrLn $ T.unlines text
sendToPager' _ text =
  forM_ text T.putStrLn

