{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : GHCup.Prelude.Internal
Description : MegaParsec utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

Stuff that doesn't need GHCup modules, so we can avoid
recursive imports.
-}
module GHCup.Prelude.Internal where


import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.List                      ( intercalate, stripPrefix, isPrefixOf, dropWhileEnd )
import           Data.Maybe
import           Data.String
import           Data.Text                      ( Text )
import           Data.Versions
import           Data.Word8                  hiding ( isDigit )
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant.Excepts
import           System.IO.Error

import           Control.Retry
import           GHC.IO.Exception

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.Strict.Maybe             as S
import qualified Data.List.Split               as Split
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Encoding.Error      as E
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Lazy.Encoding       as TLE



-- $setup
-- >>> import Data.ByteString.Internal (c2w, w2c)
-- >>> import Test.QuickCheck
-- >>> import Data.Word8
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Char as C
-- >>> import Data.List
-- >>> instance Arbitrary T.Text where arbitrary = T.pack <$> arbitrary


fS :: IsString a => String -> a
fS = fromString

fromStrictMaybe :: S.Maybe a -> Maybe a
fromStrictMaybe = S.maybe Nothing Just

fSM :: S.Maybe a -> Maybe a
fSM = fromStrictMaybe

toStrictMaybe :: Maybe a -> S.Maybe a
toStrictMaybe = maybe S.Nothing S.Just

tSM :: Maybe a -> S.Maybe a
tSM = toStrictMaybe

internalError :: String -> IO a
internalError = fail . ("Internal error: " <>)

iE :: String -> IO a
iE = internalError


showT :: Show a => a -> Text
showT = fS . show

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM ~b ~t = ifM b t (return ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM ~b ~f = ifM b (return ()) f

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM ~b ~t ~f = do
  b' <- b
  if b' then t else f

whileM :: Monad m => m a -> (a -> m Bool) -> m a
whileM ~action ~f = do
  a  <- action
  b' <- f a
  if b' then whileM action f else pure a

whileM_ :: Monad m => m a -> (a -> m Bool) -> m ()
whileM_ ~action = void . whileM action

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM ~f = guard =<< f


handleIO' :: (MonadIO m, MonadCatch m)
          => IOErrorType
          -> (IOException -> m a)
          -> m a
          -> m a
handleIO' err handler = handleIO
  (\e -> if err == ioeGetErrorType e then handler e else liftIO $ ioError e)


(??) :: forall e es a m . (Monad m, e :< es) => Maybe a -> e -> Excepts es m a
(??) m e = maybe (throwE e) pure m


(!?) :: forall e es a m
      . (Monad m, e :< es)
     => m (Maybe a)
     -> e
     -> Excepts es m a
(!?) em e = lift em >>= (?? e)


lE :: forall e es a m . (Monad m, e :< es) => Either e a -> Excepts es m a
lE = liftE . veitherToExcepts . fromEither

lE' :: forall e' e es a m
     . (Monad m, e :< es)
    => (e' -> e)
    -> Either e' a
    -> Excepts es m a
lE' f = liftE . veitherToExcepts . fromEither . first f

lEM :: forall e es a m . (Monad m, e :< es) => m (Either e a) -> Excepts es m a
lEM em = lift em >>= lE

lEM' :: forall e' e es a m
      . (Monad m, e :< es)
     => (e' -> e)
     -> m (Either e' a)
     -> Excepts es m a
lEM' f em = lift em >>= lE . first f


fromEither :: Either a b -> VEither '[a] b
fromEither = either (VLeft . V) VRight


liftIOException' :: ( MonadCatch m
                    , MonadIO m
                    , Monad m
                    , e :< es'
                    , LiftVariant es es'
                    )
                 => IOErrorType
                 -> e
                 -> Excepts es m a
                 -> Excepts es' m a
liftIOException' errType ex =
  handleIO
      (\e ->
        if errType == ioeGetErrorType e then throwE ex else liftIO $ ioError e
      )
    . liftE


liftIOException :: (MonadCatch m, MonadIO m, Monad m, e :< es')
                => IOErrorType
                -> e
                -> m a
                -> Excepts es' m a
liftIOException errType ex =
  handleIO
      (\e ->
        if errType == ioeGetErrorType e then throwE ex else liftIO $ ioError e
      )
    . lift


-- | Uses safe-exceptions.
hideError :: (MonadIO m, MonadCatch m) => IOErrorType -> m () -> m ()
hideError err = handleIO (\e -> if err == ioeGetErrorType e then pure () else liftIO . ioError $ e)


hideErrorDef :: (MonadIO m, MonadCatch m) => [IOErrorType] -> a -> m a -> m a
hideErrorDef errs def =
  handleIO (\e -> if ioeGetErrorType e `elem` errs then pure def else liftIO $ ioError e)


hideErrorDefM :: (MonadIO m, MonadCatch m) => [IOErrorType] -> m a -> m a -> m a
hideErrorDefM errs def =
  handleIO (\e -> if ioeGetErrorType e `elem` errs then def else liftIO $ ioError e)


-- TODO: does this work?
hideExcept :: forall e es es' a m
            . (Monad m, e :< es, LiftVariant (Remove e es) es')
           => e
           -> a
           -> Excepts es m a
           -> Excepts es' m a
hideExcept _ a =
  catchLiftLeft ((\_ -> pure a) :: (e -> Excepts es' m a))


hideExcept' :: forall e es es' m
             . (Monad m, e :< es, LiftVariant (Remove e es) es')
            => e
            -> Excepts es m ()
            -> Excepts es' m ()
hideExcept' _ =
  catchLiftLeft ((\_ -> pure ()) :: (e -> Excepts es' m ()))


reThrowAll :: forall e es es' a m
            . (Monad m, e :< es')
           => (V es -> e)
           -> Excepts es m a
           -> Excepts es' m a
reThrowAll f = catchAllE (throwE . f)


reThrowAllIO :: forall e es es' a m
              . (MonadCatch m, Monad m, MonadIO m, e :< es')
             => (V es -> e)
             -> (IOException -> e)
             -> Excepts es m a
             -> Excepts es' m a
reThrowAllIO f g = handleIO (throwE . g) . catchAllE (throwE . f)


throwEither :: (Exception a, MonadThrow m) => Either a b -> m b
throwEither a = case a of
  Left  e -> throwM e
  Right r -> pure r


throwEither' :: (Exception a, MonadThrow m) => a -> Either x b -> m b
throwEither' e eth = case eth of
  Left  _ -> throwM e
  Right r -> pure r

throwMaybe :: (Exception a, MonadThrow m) => a -> Maybe b -> m b
throwMaybe a m = case m of
  Nothing -> throwM a
  Just r -> pure r

throwMaybeM :: (Exception a, MonadThrow m) => a -> m (Maybe b) -> m b
throwMaybeM a am = do
  m <- am
  throwMaybe a m


verToBS :: Version -> ByteString
verToBS = E.encodeUtf8 . prettyVer

verToS :: Version -> String
verToS = T.unpack . prettyVer

intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . B.toLazyText . B.decimal



-- | Safe 'decodeUtf8With'. Replaces an invalid input byte with
-- the Unicode replacement character U+FFFD.
decUTF8Safe :: ByteString -> Text
decUTF8Safe = E.decodeUtf8With E.lenientDecode

decUTF8Safe' :: L.ByteString -> Text
decUTF8Safe' = TL.toStrict . TLE.decodeUtf8With E.lenientDecode


-- | Escape a version for use in regex
escapeVerRex :: Version -> ByteString
escapeVerRex = B.pack . go . B.unpack . verToBS
 where
  go [] = []
  go (x : xs) | x == _period = [_backslash, _period] ++ go xs
              | otherwise    = x : go xs



recover :: (MonadIO m, MonadMask m) => m a -> m a
recover action = 
  recovering (fullJitterBackoff 25000 <> limitRetries 10)
    [\_ -> Handler (\e -> pure $ isPermissionError e)
    ,\_ -> Handler (\e -> pure (ioeGetErrorType e == InappropriateType))
    ,\_ -> Handler (\e -> pure (ioeGetErrorType e == UnsatisfiedConstraints))
    ]
    (\_ -> action)


-- | Gathering monoidal values
--
-- >>> traverseFold (pure . (:["0"])) ["1","2"]
-- ["1","0","2","0"]
-- >>> traverseFold Just ["1","2","3","4","5"]
-- Just "12345"
--
-- prop> \t -> traverseFold Just t === Just (mconcat t)
traverseFold :: (Foldable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
traverseFold f = foldl (\mb a -> (<>) <$> mb <*> f a) (pure mempty)

-- | Gathering monoidal values
forFold :: (Foldable t, Applicative m, Monoid b) => t a -> (a -> m b) -> m b
forFold = \t -> (`traverseFold` t)


-- | Strip @\\r@ and @\\n@ from 'String's
--
-- >>> stripNewline "foo\n\n\n"
-- "foo"
-- >>> stripNewline "foo\n\n\nfoo"
-- "foofoo"
-- >>> stripNewline "foo\r"
-- "foo"
-- >>> stripNewline "foo"
-- "foo"
--
-- prop> \t -> stripNewline (t <> "\n") === stripNewline t
-- prop> \t -> not (any (isNewLine . c2w) t) ==> stripNewline t == t
stripNewline :: String -> String
stripNewline = filter (`notElem` "\n\r")


-- | Strip @\\r@ and @\\n@ from end of 'String'.
--
-- >>> stripNewlineEnd "foo\n\n\n"
-- "foo"
-- >>> stripNewlineEnd "foo\n\n\nfoo"
-- "foo\n\n\nfoo"
-- >>> stripNewlineEnd "foo\r"
-- "foo"
-- >>> stripNewlineEnd "foo"
-- "foo"
--
-- prop> \t -> stripNewlineEnd (t <> "\n") === stripNewlineEnd t
-- prop> \t -> not (any (isNewLine . c2w) t) ==> stripNewlineEnd t == t
stripNewlineEnd :: String -> String
stripNewlineEnd = dropWhileEnd (`elem` "\n\r")


-- | Strip @\\r@ and @\\n@ from 'Text's
--
-- >>> stripNewline' "foo\n\n\n"
-- "foo"
-- >>> stripNewline' "foo\n\n\nfoo"
-- "foofoo"
-- >>> stripNewline' "foo\r"
-- "foo"
-- >>> stripNewline' "foo"
-- "foo"
--
-- prop> \t -> stripNewline' (t <> "\n") === stripNewline' t
-- prop> \t -> not (T.any (isNewLine . c2w) t) ==> stripNewline' t == t
stripNewline' :: T.Text -> T.Text
stripNewline' = T.filter (`notElem` "\n\r")


-- | Is the word8 a newline?
--
-- >>> isNewLine (c2w '\n')
-- True
-- >>> isNewLine (c2w '\r')
-- True
--
-- prop> \w -> w /= _lf && w /= _cr ==> not (isNewLine w)
isNewLine :: Word8 -> Bool
isNewLine w
  | w == _lf = True
  | w == _cr = True
  | otherwise = False


-- | Split on a PVP suffix.
--
-- >>> splitOnPVP "-" "ghc-iserv-dyn-9.3.20210706"
-- ("ghc-iserv-dyn","9.3.20210706")
-- >>> splitOnPVP "-" "ghc-iserv-dyn"
-- ("ghc-iserv-dyn","")
splitOnPVP :: String -> String -> (String, String)
splitOnPVP c s = case Split.splitOn c s of
  []  -> def
  [_] -> def
  xs
    | let l = last xs
    , (Right _) <- pvp (T.pack l) -> (intercalate c (init xs), l)
    | otherwise -> def
 where
  def = (s, "")



-- | Like 'find', but where the test can be monadic.
--
-- >>> findM (Just . C.isUpper) "teST"
-- Just (Just 'S')
-- >>> findM (Just . C.isUpper) "test"
-- Just Nothing
-- >>> findM (Just . const True) ["x",undefined]
-- Just (Just "x")
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM ~p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)


-- | Drops the given suffix from a list.
--   It returns the original sequence if the sequence doesn't end with the given suffix.
--
-- >>> dropSuffix "!" "Hello World!"
-- "Hello World"
-- >>> dropSuffix "!" "Hello World!!"
-- "Hello World!"
-- >>> dropSuffix "!" "Hello World."
-- "Hello World."
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b

-- | Return the prefix of the second list if its suffix
--   matches the entire first list.
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
-- >>> stripSuffix ""    "baz"
-- Just "baz"
-- >>> stripSuffix "foo" "quux"
-- Nothing
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)


-- | Drops the given prefix from a list.
--   It returns the original sequence if the sequence doesn't start with the given prefix.
--
-- >>> dropPrefix "Mr. " "Mr. Men"
-- "Men"
-- >>> dropPrefix "Mr. " "Dr. Men"
-- "Dr. Men"
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b



-- | Break a list into pieces separated by the first
-- list argument, consuming the delimiter. An empty delimiter is
-- invalid, and will cause an error to be raised.
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
-- >>> splitOn "x"    "x"
-- ["",""]
-- >>> splitOn "x"    ""
-- [""]
--
-- prop> \s x -> s /= "" ==> intercalate s (splitOn s x) == x
-- prop> \c x -> splitOn [c] x                           == split (==c) x
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn, needle may not be empty"
splitOn _ [] = [[]]
splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
    where (a,b) = breakOn needle haystack


-- | Splits a list into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.
--
-- >>> split (== 'a') "aabbaca"
-- ["","","bb","c",""]
-- >>> split (== 'a') ""
-- [""]
-- >>> split (== ':') "::xyz:abc::123::"
-- ["","","xyz","abc","","123","",""]
-- >>> split (== ',') "my,list,here"
-- ["my","list","here"]
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split f (x:xs)
  | f x = [] : split f xs
  | y:ys <- split f xs = (x:y) : ys
  | otherwise = [[]]


-- | Find the first instance of @needle@ in @haystack@.
-- The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
-- If you want the remainder /without/ the match, use 'stripInfix'.
--
-- >>> breakOn "::" "a::b::c"
-- ("a","::b::c")
-- >>> breakOn "/" "foobar"
-- ("foobar","")
--
-- prop> \needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack
breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn _ [] = ([], [])
breakOn needle (x:xs) = first (x:) $ breakOn needle xs
