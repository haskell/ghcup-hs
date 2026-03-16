module GHCup.Prelude.JSON where

import qualified Data.Text as T

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs

lower :: Char -> Char
lower = T.head . T.toLower . T.singleton

upper :: Char -> Char
upper = T.head . T.toUpper . T.singleton

