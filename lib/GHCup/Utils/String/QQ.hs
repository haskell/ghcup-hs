{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : GHCup.Utils.String.QQ
Description : String quasi quoters
Copyright   : (c) Audrey Tang <audreyt@audreyt.org> 2019, Julian Ospald <hasufell@posteo.de> 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

QuasiQuoter for non-interpolated strings, texts and bytestrings.

The "s" quoter contains a multi-line string with no interpolation at all,
except that the leading newline is trimmed and carriage returns stripped.

@
{-\# LANGUAGE QuasiQuotes #-}
import Data.Text (Text)
import Data.String.QQ
foo :: Text -- "String", "ByteString" etc also works
foo = [s|
Well here is a
    multi-line string!
|]
@

Any instance of the IsString type is permitted.

(For GHC versions 6, write "[$s||]" instead of "[s||]".)

-}
module GHCup.Utils.String.QQ
  ( s
  )
where


import           Data.Char
import           GHC.Exts                       ( IsString(..) )
import           Language.Haskell.TH.Quote

-- | QuasiQuoter for a non-interpolating ASCII IsString literal.
-- The pattern portion is undefined.
s :: QuasiQuoter
s = QuasiQuoter
  (\s' -> case and $ fmap isAscii s' of
    True  -> (\a -> [|fromString a|]) . trimLeadingNewline . removeCRs $ s'
    False -> fail "Not ascii"
  )
  (error "Cannot use q as a pattern")
  (error "Cannot use q as a type")
  (error "Cannot use q as a dec")
 where
  removeCRs = filter (/= '\r')
  trimLeadingNewline ('\n' : xs) = xs
  trimLeadingNewline xs          = xs

