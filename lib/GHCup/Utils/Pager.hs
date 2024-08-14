{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

module GHCup.Utils.Pager where

import GHCup.Prelude
import System.Environment
import GHCup.Utils.Dirs (findExecutable)
import Data.Foldable (asum)
import System.Process
import System.Exit
import System.IO
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad (forM_)
import Control.Exception (IOException, try)
import GHCup.Utils.Output
import qualified Data.Text as T


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
sendToPager pager text = try @IOException
    $ withCreateProcess (shell pager) { std_in = CreatePipe
                                      , std_err = CreatePipe
                                      , std_out = Inherit
                                      , delegate_ctlc = True
                                      }
    $ \(Just stdinH) _ (Just stderrH) ph -> do
        forM_ text $ T.hPutStrLn stdinH
        hClose stdinH
        exitCode <- waitForProcess ph
        case exitCode of
          ExitFailure i ->
            do errContents <- hGetContents stderrH
               fail (unlines [mappend "Pager exited with exit code " (show i)
                             ,errContents])
          _ -> pure ()


sendToPager' :: Maybe FilePath -> [Text] -> IO (Either IOException ())
sendToPager' (Just pager) text
  | pager /= "" = do
      fits <- fitsInTerminal text
      case fits of
        Just True -> do
          T.putStr $ T.unlines text
          pure $ Right ()
        _ -> sendToPager pager text
sendToPager' _ text = do
  forM_ text T.putStrLn
  pure $ Right ()

