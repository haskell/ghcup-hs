{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE QuasiQuotes #-}
module GHCup.Prelude.String.QQ
  ( s
  )
where


import Data.Char
import GHC.Exts                  ( IsString (..) )
import Language.Haskell.TH.Quote

-- | QuasiQuoter for a non-interpolating ASCII IsString literal.
-- The pattern portion is undefined.
s :: QuasiQuoter
s = QuasiQuoter
  (\s' -> if all isAscii s'
    then (\a -> [|fromString a|]) . trimLeadingNewline . removeCRs $ s'
    else fail "Not ascii"
  )
  (error "Cannot use s as a pattern")
  (error "Cannot use s as a type")
  (error "Cannot use s as a dec")
 where
  removeCRs = filter (/= '\r')
  trimLeadingNewline ('\n' : xs) = xs
  trimLeadingNewline xs          = xs
