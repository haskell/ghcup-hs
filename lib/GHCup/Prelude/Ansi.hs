module GHCup.Prelude.Ansi where

import           Control.Applicative
import           Data.Char
import           Data.Void

import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC


-- | Calculate the render width of a string, considering
-- wide characters (counted as double width), ANSI escape codes
-- (not counted), and line breaks (in a multi-line string, the longest
-- line determines the width).
strWidth :: String -> Int
strWidth =
  maximum
    . (0 :)
    . map (foldr (\a b -> charWidth a + b) 0)
    . lines
    . stripAnsi

-- | Strip ANSI escape sequences from a string.
--
-- >>> stripAnsi "\ESC[31m-1\ESC[m"
-- "-1"
stripAnsi :: String -> String
stripAnsi s' =
  case
      MP.parseMaybe (many $ "" <$ MP.try ansi <|> pure <$> MP.anySingle) s'
    of
      Nothing -> error "Bad ansi escape"  -- PARTIAL: should not happen
      Just xs -> concat xs
 where
    -- This parses lots of invalid ANSI escape codes, but that should be fine
  ansi =
    MPC.string "\ESC[" *> digitSemicolons *> suffix MP.<?> "ansi" :: MP.Parsec
        Void
        String
        Char
  digitSemicolons = MP.takeWhileP Nothing (\c -> isDigit c || c == ';')
  suffix = MP.oneOf ['A', 'B', 'C', 'D', 'H', 'J', 'K', 'f', 'm', 's', 'u']

-- | Get the designated render width of a character: 0 for a combining
-- character, 1 for a regular character, 2 for a wide character.
-- (Wide characters are rendered as exactly double width in apps and
-- fonts that support it.) (From Pandoc.)
charWidth :: Char -> Int
charWidth c = case c of
  _ | c < '\x0300'                     -> 1
    | c >= '\x0300' && c <= '\x036F'   -> 0
    |  -- combining
      c >= '\x0370' && c <= '\x10FC'   -> 1
    | c >= '\x1100' && c <= '\x115F'   -> 2
    | c >= '\x1160' && c <= '\x11A2'   -> 1
    | c >= '\x11A3' && c <= '\x11A7'   -> 2
    | c >= '\x11A8' && c <= '\x11F9'   -> 1
    | c >= '\x11FA' && c <= '\x11FF'   -> 2
    | c >= '\x1200' && c <= '\x2328'   -> 1
    | c >= '\x2329' && c <= '\x232A'   -> 2
    | c >= '\x232B' && c <= '\x2E31'   -> 1
    | c >= '\x2E80' && c <= '\x303E'   -> 2
    | c == '\x303F'                    -> 1
    | c >= '\x3041' && c <= '\x3247'   -> 2
    | c >= '\x3248' && c <= '\x324F'   -> 1
    | -- ambiguous
      c >= '\x3250' && c <= '\x4DBF'   -> 2
    | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
    | c >= '\x4E00' && c <= '\xA4C6'   -> 2
    | c >= '\xA4D0' && c <= '\xA95F'   -> 1
    | c >= '\xA960' && c <= '\xA97C'   -> 2
    | c >= '\xA980' && c <= '\xABF9'   -> 1
    | c >= '\xAC00' && c <= '\xD7FB'   -> 2
    | c >= '\xD800' && c <= '\xDFFF'   -> 1
    | c >= '\xE000' && c <= '\xF8FF'   -> 1
    | -- ambiguous
      c >= '\xF900' && c <= '\xFAFF'   -> 2
    | c >= '\xFB00' && c <= '\xFDFD'   -> 1
    | c >= '\xFE00' && c <= '\xFE0F'   -> 1
    | -- ambiguous
      c >= '\xFE10' && c <= '\xFE19'   -> 2
    | c >= '\xFE20' && c <= '\xFE26'   -> 1
    | c >= '\xFE30' && c <= '\xFE6B'   -> 2
    | c >= '\xFE70' && c <= '\xFEFF'   -> 1
    | c >= '\xFF01' && c <= '\xFF60'   -> 2
    | c >= '\xFF61' && c <= '\x16A38'  -> 1
    | c >= '\x1B000' && c <= '\x1B001' -> 2
    | c >= '\x1D000' && c <= '\x1F1FF' -> 1
    | c >= '\x1F200' && c <= '\x1F251' -> 2
    | c >= '\x1F300' && c <= '\x1F773' -> 1
    | c >= '\x20000' && c <= '\x3FFFD' -> 2
    | otherwise                        -> 1
