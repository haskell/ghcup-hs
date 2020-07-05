{-# LANGUAGE CPP #-}

module System.OsRelease.Megaparsec where

import           Control.Applicative
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Data.Char
import           Data.Functor
import           Data.Void

import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP


-- | Parse the entire file, handling newlines and comments gracefully.
--
-- This parser generally shouldn't fail, but instead report a failed
-- parsed line as @Left@ value.
parseAssignments :: MP.Parsec
                      Void
                      String
                      [Either (MP.ParseError String Void) (String, String)]
parseAssignments =
  (\xs x -> join xs ++ x) <$> many (line MP.eol) <*> line MP.eof
 where
  line eol = choice'
    [ comment $> []
    , blank $> []
    , fmap
      (: [])
      ( MP.withRecovery (\e -> parseUntil eol $> Left e)
      . fmap Right
      $ (parseAssignment <* eol)
      )
    ]
   where
    comment = pWs *> MP.char '#' *> parseUntil eol *> eol
    blank   = pWs *> eol


-- | Parse a single line assignment and extract the right hand side.
-- This is only a subset of a shell parser, refer to the spec for
-- details.
parseAssignment :: MP.Parsec Void String (String, String)
parseAssignment =
  (,) <$> (pWs *> key) <*> (MP.char '=' *> (MP.try qval <|> mempty) <* pWs)
 where
  dropSpace :: String -> String
  dropSpace = reverse . dropWhile (\x -> x == ' ' || x == '\t') . reverse

  key :: MP.Parsec Void String String
  key = some (MP.try MP.alphaNumChar <|> MP.char '_')

  qval :: MP.Parsec Void String String
  qval = do
    c <- MP.lookAhead MP.printChar
    case c of
      ' '  -> pure ""
      '"'  -> MP.char c *> val c <* MP.char c
      '\'' -> MP.char c *> val c <* MP.char c
      -- no quote, have to drop trailing spaces
      _    -> fmap
        dropSpace
        (some $ MP.satisfy (\x -> isAlphaNum x || (x `elem` ['_', '-', '.']))) -- this is more lax than the spec
  val :: Char -> MP.Parsec Void String String
  val !q = many (qspecial q <|> MP.noneOf (specials q)) -- noneOf may be too lax

  qspecial :: Char -> MP.Parsec Void String Char
  qspecial !q =
    fmap (!! 1)
      . (\xs -> choice' xs)
      . fmap (\s -> MP.try . MP.chunk $ ['\\', s])
      $ (specials q)

  specials :: Char -> [Char]
  specials !q = [q, '\\', '$', '`']


parseUntil :: MP.Parsec Void String a -> MP.Parsec Void String String
parseUntil !p = do
  (MP.try (MP.lookAhead p) $> [])
    <|> (do
          c  <- MP.anySingle
          c2 <- parseUntil p
          pure ([c] `mappend` c2)
        )


-- | Parse one or more white spaces or tabs.
pWs :: MP.Parsec Void String ()
pWs = many (MP.satisfy (\x -> x == ' ' || x == '\t')) $> ()


-- | Try all parses in order, failing if all failed. Also fails
-- on empty list.
choice' :: (MonadFail f, MP.MonadParsec e s f) => [f a] -> f a
choice' = \case
  [] -> fail "Empty list"
  xs -> foldr1 (\x y -> MP.try x <|> MP.try y) xs
