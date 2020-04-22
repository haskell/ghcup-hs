{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}


module GHCup.Utils
  ( module GHCup.Utils.Dirs
  , module GHCup.Utils
  )
where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Utils.File
import           GHCup.Utils.Prelude

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
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
import           System.Posix.FilePath          ( getSearchPath
                                                , takeFileName
                                                )
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
ghcLinkDestination tool ver = "../ghc/" <> verToBS ver <> "/bin/" <> tool


-- | Extract the version part of the result of `ghcLinkDestination`.
ghcLinkVersion :: MonadThrow m => ByteString -> m Version
ghcLinkVersion = either (throwM . ParseError) pure . parseOnly parser
 where
  parser    = string "../ghc/" *> verParser <* string "/bin/ghc"
  verParser = many1' (notWord8 _slash) >>= \t ->
    case
        version (decUTF8Safe $ B.pack t)
      of
        Left  e -> fail $ show e
        Right r -> pure r


-- e.g. ghc-8.6.5
rmMinorSymlinks :: (MonadIO m, MonadLogger m) => Version -> m ()
rmMinorSymlinks ver = do
  bindir <- liftIO $ ghcupBinDir
  files  <- liftIO $ getDirsFiles' bindir
  let myfiles =
        filter (\x -> ("-" <> verToBS ver) `B.isSuffixOf` toFilePath x) files
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
  let v' = E.encodeUtf8 $ intToText mj <> "." <> intToText mi

  bindir <- liftIO ghcupBinDir

  files  <- liftIO $ getDirsFiles' bindir
  let myfiles = filter (\x -> ("-" <> v') `B.isSuffixOf` toFilePath x) files
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


ghcSet :: (MonadIO m) => m (Maybe Version)
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
  mc       <- liftIO $ executeOut cabalbin ["--numeric-version"] Nothing
  let reportedVer = fst . B.spanEnd (== _lf) . _stdOut $ mc
  case version $ decUTF8Safe reportedVer of
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
  semvers <- forM ghcs $ \ghc ->
    throwEither . semver =<< (throwEither . E.decodeUtf8' . toFilePath $ ghc)
  mapM (throwEither . version)
    . fmap prettySemVer
    . lastMay
    . sort
    . filter
        (\SemVer {..} ->
          fromIntegral _svMajor == major' && fromIntegral _svMinor == minor'
        )
    $ semvers


-- | Get the latest available ghc for X.Y major version.
getLatestGHCFor :: Int -- ^ major version component
                -> Int -- ^ minor version component
                -> GHCupDownloads
                -> Maybe Version
getLatestGHCFor major' minor' dls = do
  join . fmap
      (lastMay . filter
        (\v -> case semver $ prettyVer v of
                 Right SemVer{..} -> fromIntegral _svMajor == major' && fromIntegral _svMinor == minor'
                 Left _ -> False
        )
      )
    . preview (ix GHC % to Map.keys) $ dls





    -----------------
    --[ Unpacking ]--
    -----------------



-- | Unpack an archive to a temporary directory and return that path.
unpackToDir :: (MonadLogger m, MonadIO m, MonadThrow m)
            => Path Abs       -- ^ destination dir
            -> Path Abs       -- ^ archive path
            -> Excepts '[UnknownArchive] m ()
unpackToDir dest av = do
  fp <- (decUTF8Safe . toFilePath) <$> basename av
  let dfp = decUTF8Safe . toFilePath $ dest
  lift $ $(logInfo) [i|Unpacking: #{fp} to #{dfp}|]
  fn <- toFilePath <$> basename av
  let untar = Tar.unpack (toFilePath dest) . Tar.read

  -- extract, depending on file extension
  if
    | ".tar.gz" `B.isSuffixOf` fn -> liftIO
      (untar . GZip.decompress =<< readFile av)
    | ".tar.xz" `B.isSuffixOf` fn -> do
      filecontents <- liftIO $ readFile av
      let decompressed = Lzma.decompress filecontents
      liftIO $ untar decompressed
    | ".tar.bz2" `B.isSuffixOf` fn -> liftIO
      (untar . BZip.decompress =<< readFile av)
    | ".tar" `B.isSuffixOf` fn -> liftIO (untar =<< readFile av)
    | otherwise -> throwE $ UnknownArchive fn




    ------------
    --[ Tags ]--
    ------------


-- | Get the tool version that has this tag. If multiple have it,
-- picks the greatest version.
getTagged :: Tag -> AffineFold (Map.Map Version VersionInfo) (Version, VersionInfo)
getTagged tag =
  ( to (Map.filter (\VersionInfo {..} -> elem tag _viTags))
  % to Map.toDescList
  % _head
  )

getLatest :: GHCupDownloads -> Tool -> Maybe Version
getLatest av tool = headOf (ix tool % getTagged Latest % to fst) $ av

getRecommended :: GHCupDownloads -> Tool -> Maybe Version
getRecommended av tool = headOf (ix tool % getTagged Recommended % to fst) $ av


-- | Gets the latest GHC with a given base version.
getLatestBaseVersion :: GHCupDownloads -> PVP -> Maybe Version
getLatestBaseVersion av pvpVer = headOf (ix GHC % getTagged (Base pvpVer) % to fst) av



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


-- Get tool files from '~/.ghcup/bin/ghc/<ver>/bin/*'
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
    (B.stripPrefix "ghc-" . takeFileName)
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
  let mymake = if has_gmake then "gmake" else "make"
  execLogged mymake True args [rel|ghc-make|] workdir Nothing


-- | Try to apply patches in order. Fails with 'PatchFailed'
-- on first failure.
applyPatches :: (MonadLogger m, MonadIO m)
             => Path Abs   -- ^ dir containing patches
             -> Path Abs   -- ^ dir to apply patches in
             -> Excepts '[PatchFailed] m ()
applyPatches pdir ddir = do
  patches <- liftIO $ getDirsFiles pdir
  forM_ (sort patches) $ \patch' -> do
    lift $ $(logInfo) [i|Applying patch #{patch'}|]
    (fmap (either (const Nothing) Just) $ liftIO $ exec
        "patch"
        True
        ["-p1", "-i", toFilePath patch']
        (Just ddir)
        Nothing
      )
      !? PatchFailed


darwinNotarization :: Platform -> Path Abs -> IO (Either ProcessError ())
darwinNotarization Darwin path = exec
  "xattr"
  True
  ["-r", "-d", "com.apple.quarantine", toFilePath path]
  Nothing
  Nothing
darwinNotarization _ _ = pure $ Right ()


getChangeLog :: GHCupDownloads -> Tool -> Either Version Tag -> Maybe URI
getChangeLog dls tool (Left v') =
  preview (ix tool % ix v' % viChangeLog % _Just) dls
getChangeLog dls tool (Right tag) = preview
  ( ix tool
  % getTagged tag
  % to snd
  % viChangeLog
  % _Just
  ) dls
