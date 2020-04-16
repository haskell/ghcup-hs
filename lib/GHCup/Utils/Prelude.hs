{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module GHCup.Utils.Prelude where

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( lift )
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.String
import           Data.Text                      ( Text )
import           Data.Versions
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant.Excepts
import           System.IO.Error
import           System.Posix.Env.ByteString    ( getEnvironment )

import qualified Data.ByteString.Lazy          as L
import qualified Data.Strict.Maybe             as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Lazy.Encoding       as TLE



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

lBS2sT :: L.ByteString -> Text
lBS2sT = TL.toStrict . TLE.decodeUtf8



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
lE' f = liftE . veitherToExcepts . fromEither . bimap f id

lEM :: forall e es a m . (Monad m, e :< es) => m (Either e a) -> Excepts es m a
lEM em = lift em >>= lE

lEM' :: forall e' e es a m
      . (Monad m, e :< es)
     => (e' -> e)
     -> m (Either e' a)
     -> Excepts es m a
lEM' f em = lift em >>= lE . bimap f id

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


hideErrorDef :: [IOErrorType] -> a -> IO a -> IO a
hideErrorDef errs def =
  handleIO (\e -> if ioeGetErrorType e `elem` errs then pure def else ioError e)


hideErrorDefM :: [IOErrorType] -> IO a -> IO a -> IO a
hideErrorDefM errs def =
  handleIO (\e -> if ioeGetErrorType e `elem` errs then def else ioError e)


-- TODO: does this work?
hideExcept :: forall e es es' a m
            . (Monad m, e :< es, LiftVariant (Remove e es) es')
           => e
           -> a
           -> Excepts es m a
           -> Excepts es' m a
hideExcept _ a action =
  catchLiftLeft ((\_ -> pure a) :: (e -> Excepts es' m a)) action


hideExcept' :: forall e es es' m
             . (Monad m, e :< es, LiftVariant (Remove e es) es')
            => e
            -> Excepts es m ()
            -> Excepts es' m ()
hideExcept' _ action =
  catchLiftLeft ((\_ -> pure ()) :: (e -> Excepts es' m ())) action


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


verToBS :: Version -> ByteString
verToBS = E.encodeUtf8 . prettyVer


intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . B.toLazyText . B.decimal


removeLensFieldLabel :: String -> String
removeLensFieldLabel str' =
  maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str'


addToCurrentEnv :: MonadIO m
                => [(ByteString, ByteString)]
                -> m [(ByteString, ByteString)]
addToCurrentEnv adds = do
  cEnv <- liftIO $ getEnvironment
  pure (adds ++ cEnv)


pvpToVersion :: PVP -> Version
pvpToVersion = either (\_ -> error "Couldn't convert PVP to Version") id . version . prettyPVP
