module Main where

import Balls

import Terminal.Game

-- Balls Main module. The meat of the game is in `examples/Balls.hs`

main :: IO ()
main = getStdGen >>= \g ->
       playGame (fireworks g)

