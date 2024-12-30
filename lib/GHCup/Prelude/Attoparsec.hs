{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-|
Module      : GHCup.Utils.Attoparsec
Description : Attoparsec utilities
Copyright   : (c) Julian Ospald, 2024
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Prelude.Attoparsec where

import           Control.Applicative
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (w2c)
import           Data.Functor

import           Data.Attoparsec.ByteString as AP



parseList :: Parser a -> Parser [a]
parseList p = skipSpaces *> string "[" *> skipSpaces *> loop
 where
  loop = (:) <$> p <*> (     (skipSpaces *> string "," *> skipSpaces *> loop)
                         <|> ([] <$ skipSpaces <* string "]")
                       )

parseList' :: Parser [ByteString]
parseList' = skipSpaces *> string "[" *> skipSpaces *> loop
 where
  loop = (:) <$> AP.takeWhile (\c -> w2c c `notElem` [',',']'] && not (isSpace $ w2c c)) <*> ((skipSpaces *> string "," *> skipSpaces *> loop)
                         <|> ([] <$ skipSpaces <* string "]")
                       )

skipSpace :: Parser ()
skipSpace = void $ satisfy (isSpace . w2c)

skipSpaces :: Parser ()
skipSpaces = void $ many skipSpace

isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

