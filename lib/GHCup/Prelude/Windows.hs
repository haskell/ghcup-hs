{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module GHCup.Prelude.Windows where


import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Data.Bits

import           System.Win32.Console
import           System.Win32.File     hiding ( copyFile )
import           System.Win32.Types




-- | Enables ANSI support on windows, does nothing on unix.
--
-- Returns 'Left str' on errors and 'Right bool' on success, where
-- 'bool' marks whether ansi support was already enabled.
--
-- This function never crashes.
--
-- Rip-off of https://docs.rs/ansi_term/0.12.1/x86_64-pc-windows-msvc/src/ansi_term/windows.rs.html#10-61
enableAnsiSupport :: IO (Either String Bool)
enableAnsiSupport = handleIO (pure . Left . displayException) $ do
  -- ref: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
  -- Using `CreateFileW("CONOUT$", ...)` to retrieve the console handle works correctly even if STDOUT and/or STDERR are redirected
  h <- createFile "CONOUT$" (gENERIC_WRITE .|. gENERIC_READ)
    fILE_SHARE_WRITE Nothing oPEN_EXISTING 0 Nothing
  when (h == iNVALID_HANDLE_VALUE ) $ fail "invalid handle value"

  -- ref: https://docs.microsoft.com/en-us/windows/console/getconsolemode
  m <- getConsoleMode h

  -- VT processing not already enabled?
  if m .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING == 0
  -- https://docs.microsoft.com/en-us/windows/console/setconsolemode
  then setConsoleMode h (m .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING)
    >> pure (Right False)
  else pure (Right True)


isWindows, isNotWindows :: Bool
isWindows = True
isNotWindows = not isWindows

