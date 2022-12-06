module Terminal.Game.Character where

import Data.Char        as C
import Text.Unidecode   as D
import System.IO.Unsafe as U


import Terminal.Game.Utils

-- Non ASCII character still cause crashes on Win32 console (see this
-- report: https://gitlab.haskell.org/ghc/ghc/issues/7593 ).
-- We provide a function to substitute them when playing on Win32
-- console, with another appropriate chatacter.

win32SafeChar :: Char -> Char
win32SafeChar c | areWeWin32 = toASCII c
                | otherwise  = c
    where
          areWeWin32 :: Bool
          areWeWin32 = unsafePerformIO isWin32Console

-- ANCILLARIES --

toASCII :: Char -> Char
toASCII c | C.isAscii c         = c
          | Just cm <- lu       = cm    -- hand-made substitution
          | [cu] <- unidecode c = cu    -- unidecode
          | otherwise           = '?'   -- all else failing
    where
          lu = lookup c subDictionary

subDictionary :: [(Char, Char)]
subDictionary = [ -- various open/close quotes
                  ('«', '<'),
                  ('»', '>'),
                  ('“', '\''),
                  ('”', '\''),
                  ('‘', '\''),
                  ('’', '\''),

                  -- typographical marks
                  ('—', '-') ]

