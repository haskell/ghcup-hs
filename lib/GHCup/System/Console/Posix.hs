module GHCup.System.Console.Posix where


-- | Enables ANSI support on windows, does nothing on unix.
--
-- Returns 'Left str' on errors and 'Right bool' on success, where
-- 'bool' markes whether ansi support was already enabled.
--
-- This function never crashes.
--
-- Rip-off of https://docs.rs/ansi_term/0.12.1/x86_64-pc-windows-msvc/src/ansi_term/windows.rs.html#10-61
enableAnsiSupport :: IO (Either String Bool)
enableAnsiSupport = pure (Right True)

