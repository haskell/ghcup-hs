{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : GHCup.Utils.Prelude
Description : MegaParsec utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

GHCup specific prelude. Lots of Excepts functionality.
-}
module GHCup.Utils.Prelude where

#if defined(IS_WINDOWS)
import           GHCup.Types
#endif
import           GHCup.Types.Optics

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import           Data.List                      ( nub )
import           Data.Foldable
import           Data.String
import           Data.Text                      ( Text )
import           Data.Versions
import           Data.Word8
import           Haskus.Utils.Types.List
import           Haskus.Utils.Variant.Excepts
import           System.IO.Error
#if defined(IS_WINDOWS)
import           System.IO.Temp
#endif
import           System.IO.Unsafe
import           System.Directory
import           System.FilePath

#if defined(IS_WINDOWS)
import           Control.Retry
import           GHC.IO.Exception
#endif

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import qualified Data.Strict.Maybe             as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.Encoding.Error      as E
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Lazy.Encoding       as TLE
#if defined(IS_WINDOWS)
import qualified System.Win32.File             as Win32
#endif



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


verToBS :: Version -> ByteString
verToBS = E.encodeUtf8 . prettyVer

verToS :: Version -> String
verToS = T.unpack . prettyVer

intToText :: Integral a => a -> T.Text
intToText = TL.toStrict . B.toLazyText . B.decimal


removeLensFieldLabel :: String -> String
removeLensFieldLabel str' =
  maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str'


pvpToVersion :: PVP -> Version
pvpToVersion =
  either (\_ -> error "Couldn't convert PVP to Version") id
    . version
    . prettyPVP


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

-- | More permissive version of 'createDirRecursive'. This doesn't
-- error when the destination is a symlink to a directory.
createDirRecursive' :: FilePath -> IO ()
createDirRecursive' p =
  handleIO (\e -> if isAlreadyExistsError e then isSymlinkDir e else throwIO e)
    . createDirectoryIfMissing True
    $ p

 where
  isSymlinkDir e = do
    ft <- pathIsSymbolicLink p
    case ft of
      True -> do
        rp <- canonicalizePath p
        rft <- doesDirectoryExist rp
        case rft of
          True -> pure ()
          _ -> throwIO e
      _ -> throwIO e


-- | Recursively copy the contents of one directory to another path.
--
-- This is a rip-off of Cabal library.
copyDirectoryRecursive :: FilePath -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO ()
copyDirectoryRecursive srcDir destDir doCopy = do
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith destDir [ (srcDir, f)
                          | f <- srcFiles ]
  where
    -- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
    -- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
    copyFilesWith :: FilePath -> [(FilePath, FilePath)] -> IO ()
    copyFilesWith targetDir srcFiles = do

      -- Create parent directories for everything
      let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
      traverse_ (createDirectoryIfMissing True) dirs

      -- Copy all the files
      sequence_ [ let src  = srcBase   </> srcFile
                      dest = targetDir </> srcFile
                   in doCopy src dest
                | (srcBase, srcFile) <- srcFiles ]


-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False


-- https://github.com/haskell/directory/issues/110
-- https://github.com/haskell/directory/issues/96
-- https://www.sqlite.org/src/info/89f1848d7f
recyclePathForcibly :: ( MonadIO m
                       , MonadReader env m
                       , HasDirs env
                       , MonadMask m
                       )
                    => FilePath
                    -> m ()
recyclePathForcibly fp = do
#if defined(IS_WINDOWS)
  Dirs { recycleDir } <- getDirs
  tmp <- liftIO $ createTempDirectory recycleDir "recyclePathForcibly"
  let dest = tmp </> takeFileName fp
  liftIO (Win32.moveFileEx fp (Just dest) 0)
      `catch`
      (\e -> if isPermissionError e {- EXDEV on windows -} then recover (liftIO $ removePathForcibly fp) else throwIO e)
      `finally`
        (liftIO $ handleIO (\_ -> pure ()) $ removePathForcibly tmp)
#else
  liftIO $ removePathForcibly fp
#endif


rmPathForcibly :: ( MonadIO m
                  , MonadMask m
                  )
               => FilePath
               -> m ()
rmPathForcibly fp =
#if defined(IS_WINDOWS)
  recover (liftIO $ removePathForcibly fp)
#else
  liftIO $ removePathForcibly fp
#endif


rmDirectory :: (MonadIO m, MonadMask m)
            => FilePath
            -> m ()
rmDirectory fp =
#if defined(IS_WINDOWS)
  recover (liftIO $ removeDirectory fp)
#else
  liftIO $ removeDirectory fp
#endif


-- https://www.sqlite.org/src/info/89f1848d7f
-- https://github.com/haskell/directory/issues/96
recycleFile :: ( MonadIO m
               , MonadMask m
               , MonadReader env m
               , HasDirs env
               )
            => FilePath
            -> m ()
recycleFile fp = do
#if defined(IS_WINDOWS)
  Dirs { recycleDir } <- getDirs
  liftIO $ whenM (doesDirectoryExist fp) $ ioError (IOError Nothing InappropriateType "recycleFile" "" Nothing (Just fp))
  tmp <- liftIO $ createTempDirectory recycleDir "recycleFile"
  let dest = tmp </> takeFileName fp
  liftIO (Win32.moveFileEx fp (Just dest) 0)
    `catch`
      (\e -> if isPermissionError e {- EXDEV on windows -} then recover (liftIO $ removePathForcibly fp) else throwIO e)
    `finally`
      (liftIO $ handleIO (\_ -> pure ()) $ removePathForcibly tmp)
#else
  liftIO $ removeFile fp
#endif


rmFile :: ( MonadIO m
          , MonadMask m
          )
      => FilePath
      -> m ()
rmFile fp =
#if defined(IS_WINDOWS)
  recover (liftIO $ removeFile fp)
#else
  liftIO $ removeFile fp
#endif


rmDirectoryLink :: (MonadIO m, MonadMask m, MonadReader env m, HasDirs env)
                => FilePath
                -> m ()
rmDirectoryLink fp = 
#if defined(IS_WINDOWS)
  recover (liftIO $ removeDirectoryLink fp)
#else
  liftIO $ removeDirectoryLink fp
#endif


#if defined(IS_WINDOWS)
recover :: (MonadIO m, MonadMask m) => m a -> m a
recover action = 
  recovering (fullJitterBackoff 25000 <> limitRetries 10)
    [\_ -> Handler (\e -> pure $ isPermissionError e)
    ,\_ -> Handler (\e -> pure (ioeGetErrorType e == InappropriateType))
    ,\_ -> Handler (\e -> pure (ioeGetErrorType e == UnsatisfiedConstraints))
    ]
    (\_ -> action)
#endif


-- Gathering monoidal values
traverseFold :: (Foldable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
traverseFold f = foldl (\mb a -> (<>) <$> mb <*> f a) (pure mempty)

-- | Gathering monoidal values
forFold :: (Foldable t, Applicative m, Monoid b) => t a -> (a -> m b) -> m b
forFold = \t -> (`traverseFold` t)


-- | Strip @\\r@ and @\\n@ from 'ByteString's
stripNewline :: String -> String
stripNewline s
  | null s               = []
  | head s `elem` "\n\r" = stripNewline (tail s)
  | otherwise            = head s : stripNewline (tail s)


-- | Strip @\\r@ and @\\n@ from 'ByteString's
stripNewline' :: T.Text -> T.Text
stripNewline' s
  | T.null s               = mempty
  | T.head s `elem` "\n\r" = stripNewline' (T.tail s)
  | otherwise              = T.singleton (T.head s) <> stripNewline' (T.tail s)


isNewLine :: Word8 -> Bool
isNewLine w
  | w == _lf = True
  | w == _cr = True
  | otherwise = False
