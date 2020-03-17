{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}


module GHCup.Utils
  ( module GHCup.Utils.Dirs
  , module GHCup.Utils
  )
where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Utils.File
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Class      ( lift )
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Versions
import           Data.Word8
import           GHC.IO.Exception
import           HPath
import           HPath.IO
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           Safe
import           System.IO.Error
import           System.Posix.FilePath          ( getSearchPath, takeFileName )
import           System.Posix.Files.ByteString  ( readSymbolicLink )
import           URI.ByteString

import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as Map
import qualified Data.Text.Encoding            as E






    ------------------------
    --[ Symlink handling ]--
    ------------------------


-- | The symlink destination of a ghc tool.
ghcLinkDestination :: ByteString -- ^ the tool, such as 'ghc', 'haddock' etc.
                   -> Version
                   -> ByteString
ghcLinkDestination tool ver = [s|../ghc/|] <> verToBS ver <> [s|/bin/|] <> tool


-- | Extract the version part of the result of `ghcLinkDestination`.
ghcLinkVersion :: MonadThrow m => ByteString -> m Version
ghcLinkVersion = either (throwM . ParseError) pure . parseOnly parser
 where
  parser    = string [s|../ghc/|] *> verParser <* string [s|/bin/ghc|]
  verParser = many1' (notWord8 _slash) >>= \t ->
    case version $ E.decodeUtf8 $ B.pack t of
      Left  e -> fail $ show e
      Right r -> pure r


-- e.g. ghc-8.6.5
rmMinorSymlinks :: (MonadIO m, MonadLogger m) => Version -> m ()
rmMinorSymlinks ver = do
  bindir <- liftIO $ ghcupBinDir
  files  <- liftIO $ getDirsFiles' bindir
  let myfiles =
        filter (\x -> ([s|-|] <> verToBS ver) `B.isSuffixOf` toFilePath x) files
  forM_ myfiles $ \f -> do
    let fullF = (bindir </> f)
    $(logDebug) [i|rm -f #{toFilePath fullF}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile fullF

-- E.g. ghc, if this version is the set one.
-- This reads `ghcupGHCDir`.
rmPlain :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
        => Version
        -> Excepts '[NotInstalled] m ()
rmPlain ver = do
  files  <- liftE $ ghcToolFiles ver
  bindir <- liftIO $ ghcupBinDir
  forM_ files $ \f -> do
    let fullF = (bindir </> f)
    lift $ $(logDebug) [i|rm -f #{toFilePath fullF}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile fullF
  -- old ghcup
  let hdc_file = (bindir </> [rel|haddock-ghc|])
  lift $ $(logDebug) [i|rm -f #{toFilePath hdc_file}|]
  liftIO $ hideError doesNotExistErrorType $ deleteFile hdc_file

-- e.g. ghc-8.6
rmMajorSymlinks :: (MonadLogger m, MonadIO m) => Version -> m ()
rmMajorSymlinks ver = do
  (mj, mi) <- liftIO $ getGHCMajor ver
  let v' = E.encodeUtf8 $ intToText mj <> [s|.|] <> intToText mi

  bindir <- liftIO ghcupBinDir

  files  <- liftIO $ getDirsFiles' bindir
  let myfiles = filter (\x -> ([s|-|] <> v') `B.isSuffixOf` toFilePath x) files
  forM_ myfiles $ \f -> do
    let fullF = (bindir </> f)
    $(logDebug) [i|rm -f #{toFilePath fullF}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile fullF




    -----------------------------------
    --[ Set/Installed introspection ]--
    -----------------------------------


toolAlreadyInstalled :: Tool -> Version -> IO Bool
toolAlreadyInstalled tool ver = case tool of
  GHC   -> ghcInstalled ver
  Cabal -> cabalInstalled ver
  GHCup -> pure True


ghcInstalled :: Version -> IO Bool
ghcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  doesDirectoryExist ghcdir


ghcSrcInstalled :: Version -> IO Bool
ghcSrcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  doesFileExist (ghcdir </> ghcUpSrcBuiltFile)


ghcSet :: (MonadIO m, MonadThrow m) => m (Maybe Version)
ghcSet = do
  ghcBin <- (</> [rel|ghc|]) <$> liftIO ghcupBinDir

  -- link destination is of the form ../ghc/<ver>/bin/ghc
  liftIO $ handleIO' NoSuchThing (\_ -> pure $ Nothing) $ do
    link <- readSymbolicLink $ toFilePath ghcBin
    Just <$> ghcLinkVersion link


cabalInstalled :: Version -> IO Bool
cabalInstalled ver = do
  reportedVer <- cabalSet
  pure (reportedVer == ver)

cabalSet :: (MonadIO m, MonadThrow m) => m Version
cabalSet = do
  cabalbin <- (</> [rel|cabal|]) <$> liftIO ghcupBinDir
  mc       <- liftIO $ executeOut cabalbin [[s|--numeric-version|]] Nothing
  let reportedVer = fst . B.spanEnd (== _lf) . _stdOut $ mc
  case version (E.decodeUtf8 reportedVer) of
    Left  e -> throwM e
    Right r -> pure r



    -----------------------------------------
    --[ Major version introspection (X.Y) ]--
    -----------------------------------------


-- | We assume GHC is in semver format. I hope it is.
getGHCMajor :: MonadThrow m => Version -> m (Int, Int)
getGHCMajor ver = do
  SemVer {..} <- throwEither (semver $ prettyVer ver)
  pure (fromIntegral _svMajor, fromIntegral _svMinor)


-- | Get the latest installed full GHC version that satisfies X.Y.
-- This reads `ghcupGHCBaseDir`.
getGHCForMajor :: (MonadIO m, MonadThrow m)
               => Int -- ^ major version component
               -> Int -- ^ minor version component
               -> m (Maybe Version)
getGHCForMajor major' minor' = do
  p       <- liftIO $ ghcupGHCBaseDir
  ghcs    <- liftIO $ getDirsFiles' p
  semvers <- forM ghcs $ throwEither . semver . E.decodeUtf8 . toFilePath
  mapM (throwEither . version)
    . fmap prettySemVer
    . lastMay
    . sort
    . filter
        (\SemVer {..} ->
          fromIntegral _svMajor == major' && fromIntegral _svMinor == minor'
        )
    $ semvers




    -----------------
    --[ Unpacking ]--
    -----------------



-- | Unpack an archive to a temporary directory and return that path.
unpackToDir :: (MonadLogger m, MonadIO m, MonadThrow m)
            => Path Abs       -- ^ destination dir
            -> Path Abs       -- ^ archive path
            -> Excepts '[UnknownArchive] m ()
unpackToDir dest av = do
  let fp = E.decodeUtf8 (toFilePath av)
  lift $ $(logInfo) [i|Unpacking: #{fp}|]
  fn <- toFilePath <$> basename av
  let untar = Tar.unpack (toFilePath dest) . Tar.read

  -- extract, depending on file extension
  if
    | [s|.tar.gz|] `B.isSuffixOf` fn -> liftIO
      (untar . GZip.decompress =<< readFile av)
    | [s|.tar.xz|] `B.isSuffixOf` fn -> do
      filecontents <- liftIO $ readFile av
      let decompressed = Lzma.decompress filecontents
      liftIO $ untar decompressed
    | [s|.tar.bz2|] `B.isSuffixOf` fn -> liftIO
      (untar . BZip.decompress =<< readFile av)
    | [s|.tar|] `B.isSuffixOf` fn -> liftIO (untar =<< readFile av)
    | otherwise -> throwE $ UnknownArchive fn




    ------------
    --[ Tags ]--
    ------------


-- | Get the tool versions that have this tag.
getTagged :: GHCupDownloads -> Tool -> Tag -> [Version]
getTagged av tool tag = toListOf
  ( ix tool
  % to (Map.filter (\VersionInfo {..} -> elem tag _viTags))
  % to Map.keys
  % folded
  )
  av

getLatest :: GHCupDownloads -> Tool -> Maybe Version
getLatest av tool = headOf folded $ getTagged av tool Latest

getRecommended :: GHCupDownloads -> Tool -> Maybe Version
getRecommended av tool = headOf folded $ getTagged av tool Recommended



    -----------------------
    --[ Settings Getter ]--
    -----------------------


getCache :: MonadReader Settings m => m Bool
getCache = ask <&> cache



    -------------
    --[ Other ]--
    -------------


urlBaseName :: MonadThrow m
            => ByteString  -- ^ the url path (without scheme and host)
            -> m (Path Rel)
urlBaseName = parseRel . snd . B.breakEnd (== _slash) . urlDecode False


-- Get tool files from ~/.ghcup/bin/ghc/<ver>/bin/*
-- while ignoring *-<ver> symlinks.
--
-- Returns unversioned relative files, e.g.:
--   ["hsc2hs","haddock","hpc","runhaskell","ghc","ghc-pkg","ghci","runghc","hp2ps"]
ghcToolFiles :: (MonadThrow m, MonadFail m, MonadIO m)
             => Version
             -> Excepts '[NotInstalled] m [Path Rel]
ghcToolFiles ver = do
  ghcdir <- liftIO $ ghcupGHCDir ver
  let bindir = ghcdir </> [rel|bin|]

  -- fail if ghc is not installed
  whenM (fmap not $ liftIO $ doesDirectoryExist ghcdir)
        (throwE (NotInstalled GHC ver))

  files         <- liftIO $ getDirsFiles' bindir
  -- figure out the <ver> suffix, because this might not be `Version` for
  -- alpha/rc releases, but x.y.a.somedate.
  (Just symver) <-
    (B.stripPrefix [s|ghc-|] . takeFileName)
      <$> (liftIO $ readSymbolicLink $ toFilePath (bindir </> [rel|ghc|]))
  when (B.null symver)
       (throwIO $ userError $ "Fatal: ghc symlink target is broken")

  pure $ filter (\x -> not $ symver `B.isSuffixOf` toFilePath x) files


-- | This file, when residing in ~/.ghcup/ghc/<ver>/ signals that
-- this GHC was built from source. It contains the build config.
ghcUpSrcBuiltFile :: Path Rel
ghcUpSrcBuiltFile = [rel|.ghcup_src_built|]


-- | Calls gmake if it exists in PATH, otherwise make.
make :: [ByteString] -> Maybe (Path Abs) -> IO (Either ProcessError ())
make args workdir = do
  spaths    <- catMaybes . fmap parseAbs <$> getSearchPath
  has_gmake <- isJust <$> searchPath spaths [rel|gmake|]
  let mymake = if has_gmake then [s|gmake|] else [s|make|]
  execLogged mymake True args [rel|ghc-make.log|] workdir Nothing
