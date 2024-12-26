{-# LANGUAGE CPP #-}
-- | Compatibility shim between optparse-applicative <0.18.0 and >=0.18.0
-- In moving away from ansi-wl-pprint to prettyprinter, many monomorphic
-- pretty-printers were subsumed under the polymorphic pretty :: a -> Doc ann
-- This module provides the monomorphic prettyprinters GHCup.Optparse.* use.

module Options.Applicative.Pretty.Shim (
  text,
  module Options.Applicative.Help.Pretty
) where

import Options.Applicative.Help.Pretty

#if MIN_VERSION_optparse_applicative(0,18,0)
text :: String -> Doc
text = pretty
#endif
