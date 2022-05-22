{-# LANGUAGE CPP #-}

{-|
Module      : GHCup.Utils.Process
Description : Process handling
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Prelude.Process (
  executeOut,
  execLogged,
  exec,
  toProcessError,
) where


#if IS_WINDOWS
import GHCup.Prelude.Process.Windows
#else
import GHCup.Prelude.Process.Posix
#endif

