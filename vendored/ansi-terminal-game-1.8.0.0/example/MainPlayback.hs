module Main where

import Alone ( aloneInARoom, sizeCheck )

import Terminal.Game

import System.IO.Temp ( emptySystemTempFile )

-- plays the game and, once you quit, shows a replay of the session
-- run with: cabal new-run -f examples alone-playback

main :: IO ()
main = do
        sizeCheck
        tf <- emptySystemTempFile "alone-record.gr"
        playback tf

playback :: FilePath -> IO ()
playback f = do
        prompt "Press <Enter> to play the game."
        recordGame aloneInARoom f
        prompt "Press <Enter> to watch playback."
        es <- readRecord f
        _ <- narrateGame aloneInARoom es
        prompt "Playback over! Press <Enter> to quit."
    where
          prompt :: String -> IO ()
          prompt s = putStrLn s >> () <$ getLine


