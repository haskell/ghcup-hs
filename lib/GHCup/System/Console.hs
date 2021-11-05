{-# LANGUAGE CPP #-}

module GHCup.System.Console (
#if IS_WINDOWS
  module GHCup.System.Console.Windows
#else
  module GHCup.System.Console.Posix
#endif
) where


#if IS_WINDOWS
import GHCup.System.Console.Windows
#else
import GHCup.System.Console.Posix
#endif
