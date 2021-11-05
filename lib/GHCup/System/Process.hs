{-# LANGUAGE CPP #-}

module GHCup.System.Process (
  module GHCup.System.Process.Common,
#if IS_WINDOWS
  module GHCup.System.Process.Windows
#else
  module GHCup.System.Process.Posix
#endif
) where


#if IS_WINDOWS
import GHCup.System.Process.Windows
#else
import GHCup.System.Process.Posix
#endif

import GHCup.System.Process.Common
