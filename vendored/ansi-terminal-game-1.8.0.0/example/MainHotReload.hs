module Main where

import Alone ( aloneInARoom, sizeCheck )

import Terminal.Game

-- Hot reloading is a handy feature while writing a game. Here I will
-- show you how to do that with ansi-terminal-game.
--
-- 1. install `entr` from your repositories;
-- 2. run `find example/*.hs | entr -cr cabal run -f examples hot-reload`;
-- 3. now modify example/Alone.hs and see your changes live!
--
-- Caveat: entr and similar applications do *not* work with interactive
-- programs, so you need — as shown below — to load a record and play
-- it as a demo.
-- This is still useful to iteratively build NPCs’ behaviour, GUIs, etc.
--
-- Remember that you can use `recordGame` to record a session. If you
-- need something fancier for your game (e.g. hot-reload with input),
-- `venzone` [1] (module Watcher) has a builtin /watch mode/ you can
-- take inspiration from.
--
-- [1] https://hackage.haskell.org/package/venzone

main :: IO ()
main = do
        sizeCheck
        gr <- readRecord "test/records/alone-record-test.gr"
                -- check `readRecord
        () <$ narrateGame aloneInARoom gr
