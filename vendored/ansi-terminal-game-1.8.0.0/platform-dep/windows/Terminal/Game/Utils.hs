--------------------------------------------------------------------------------
-- Nonbuffering getChar et al
-- 2017 Francesco Ariis GPLv3                                             80cols
--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}

-- horrible horrible horrible hack to make unbuffered input
-- work on Windows (and win32console check)

module Terminal.Game.Utils (inputCharTerminal,
                            isWin32Console )
                           where

import qualified Data.Char             as C
import qualified Foreign.C.Types       as FT
import qualified System.Console.MinTTY as M

inputCharTerminal :: IO Char
inputCharTerminal = getCharWindows

-- no idea why, but unsafe breaks it
getCharWindows :: IO Char
getCharWindows = fmap (C.chr . fromEnum) c_getch
foreign import ccall safe "conio.h getch"
  c_getch :: IO FT.CInt

-- not perfect, but it is what it is (on win, non minTTY)
isWin32Console :: IO Bool
isWin32Console = not <$> M.isMinTTY
