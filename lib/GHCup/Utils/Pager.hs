{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

module GHCup.Utils.Pager where

import System.Environment
import GHCup.Utils.Dirs (findExecutable)
import Data.Foldable (asum)
import System.Process
import System.Exit
import System.IO
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad (forM_, (<=<))
import Control.Exception (IOException, try)
import GHCup.Utils.Output
import qualified Data.Text as T


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
    $ \(Just stdinH) _ _ ph -> do
        forM_ text $ T.hPutStrLn stdinH
        hClose stdinH
        exitCode <- waitForProcess ph
        case exitCode of
          ExitFailure i -> fail ("Pager exited with exit code " <> show i)
          _ -> pure ()


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

