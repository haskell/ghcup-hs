{-# LANGUAGE CPP #-}

module GHCup.Utils.File (
  module GHCup.Utils.File.Common,
#if IS_WINDOWS
  module GHCup.Utils.File.Windows
#else
  module GHCup.Utils.File.Posix
#endif
) where

import GHCup.Utils.File.Common
#if IS_WINDOWS
import GHCup.Utils.File.Windows
#else
import GHCup.Utils.File.Posix
#endif
