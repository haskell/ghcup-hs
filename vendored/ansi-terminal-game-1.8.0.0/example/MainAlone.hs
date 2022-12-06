module Main where


import Alone ( aloneInARoom, sizeCheck )

import Terminal.Game

-- run with: cabal new-run -f examples alone

main :: IO ()
main = do sizeCheck
          errorPress $ playGame aloneInARoom
