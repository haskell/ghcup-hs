--------------------------------------------------------------------------------
-- Nonbuffering getChar
-- 2017 Francesco Ariis GPLv3                                             80cols
--------------------------------------------------------------------------------

module Terminal.Game.Utils ( inputCharTerminal,
                             isWin32Console )
                           where

inputCharTerminal :: IO Char
inputCharTerminal = getChar

isWin32Console :: IO Bool
isWin32Console = return False
