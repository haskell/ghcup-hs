{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}


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
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
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
import           System.Posix.Files.ByteString  ( getFileStatus, isSymbolicLink, readSymbolicLink )
import           Text.Regex.Posix
import           URI.ByteString

import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as Map
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP





    ------------------------
    --[ Symlink handling ]--
    ------------------------


-- | The symlink destination of a ghc tool.
ghcLinkDestination :: ByteString -- ^ the tool, such as 'ghc', 'haddock' etc.
                   -> GHCTargetVersion
                   -> ByteString
ghcLinkDestination tool ver =
  "../ghc/" <> E.encodeUtf8 (prettyTVer ver) <> "/bin/" <> tool


-- e.g. ghc-8.6.5
rmMinorSymlinks :: (MonadIO m, MonadLogger m) => GHCTargetVersion -> m ()
rmMinorSymlinks GHCTargetVersion {..} = do
  bindir <- liftIO $ ghcupBinDir

  files  <- liftIO $ findFiles'
    bindir
    (  maybe mempty (\x -> MP.chunk (x <> "-")) _tvTarget
    *> parseUntil1 (MP.chunk $ prettyVer _tvVersion)
    *> (MP.chunk $ prettyVer _tvVersion)
    *> MP.eof
    )

  forM_ files $ \f -> do
    let fullF = (bindir </> f)
    $(logDebug) [i|rm -f #{toFilePath fullF}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile fullF


-- Removes the set ghc version for the given target, if any.
rmPlain :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
  => Maybe Text -- ^ target
        -> Excepts '[NotInstalled] m ()
rmPlain target = do
  mtv <- ghcSet target
  forM_ mtv $ \tv -> do
    files  <- liftE $ ghcToolFiles tv
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
rmMajorSymlinks :: (MonadThrow m, MonadLogger m, MonadIO m)
                => GHCTargetVersion
                -> m ()
rmMajorSymlinks GHCTargetVersion {..} = do
  (mj, mi) <- getMajorMinorV _tvVersion
  let v' = intToText mj <> "." <> intToText mi

  bindir <- liftIO ghcupBinDir

  files  <- liftIO $ findFiles'
    bindir
    (  maybe mempty (\x -> MP.chunk (x <> "-")) _tvTarget
    *> parseUntil1 (MP.chunk v')
    *> MP.chunk v'
    *> MP.eof
    )

  forM_ files $ \f -> do
    let fullF = (bindir </> f)
    $(logDebug) [i|rm -f #{toFilePath fullF}|]
    liftIO $ hideError doesNotExistErrorType $ deleteFile fullF




    -----------------------------------
    --[ Set/Installed introspection ]--
    -----------------------------------


ghcInstalled :: GHCTargetVersion -> IO Bool
ghcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  doesDirectoryExist ghcdir


ghcSrcInstalled :: GHCTargetVersion -> IO Bool
ghcSrcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  doesFileExist (ghcdir </> ghcUpSrcBuiltFile)


ghcSet :: (MonadThrow m, MonadIO m)
       => Maybe Text   -- ^ the target of the GHC version, if any
                       --  (e.g. armv7-unknown-linux-gnueabihf)
       -> m (Maybe GHCTargetVersion)
ghcSet mtarget = do
  ghc    <- parseRel $ E.encodeUtf8 (maybe "ghc" (<> "-ghc") mtarget)
  ghcBin <- (</> ghc) <$> liftIO ghcupBinDir

  -- link destination is of the form ../ghc/<ver>/bin/ghc
  -- for old ghcup, it is ../ghc/<ver>/bin/ghc-<ver>
  liftIO $ handleIO' NoSuchThing (\_ -> pure $ Nothing) $ do
    link <- readSymbolicLink $ toFilePath ghcBin
    Just <$> ghcLinkVersion link
 where
  ghcLinkVersion :: MonadThrow m => ByteString -> m GHCTargetVersion
  ghcLinkVersion bs = do
    t <- throwEither $ E.decodeUtf8' bs
    throwEither $ MP.parse parser "" t
   where
    parser =
      MP.chunk "../ghc/"
        *> (do
             r    <- parseUntil1 (MP.chunk "/")
             rest <- MP.getInput
             MP.setInput r
             x <- ghcTargetVerP
             MP.setInput rest
             pure x
           )
        <* MP.chunk "/"
        <* MP.takeRest
        <* MP.eof


-- | Get all installed GHCs by reading ~/.ghcup/ghc/<dir>.
-- If a dir cannot be parsed, returns left.
getInstalledGHCs :: MonadIO m => m [Either (Path Rel) GHCTargetVersion]
getInstalledGHCs = do
  ghcdir <- liftIO $ ghcupGHCBaseDir
  fs     <- liftIO $ hideErrorDef [NoSuchThing] [] $ getDirsFiles' ghcdir
  forM fs $ \f -> case parseGHCupGHCDir f of
    Right r -> pure $ Right r
    Left  _ -> pure $ Left f


getInstalledCabals :: IO [Either (Path Rel) Version]
getInstalledCabals = do
  bindir <- liftIO $ ghcupBinDir
  bins   <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    bindir
    (makeRegexOpts compExtended execBlank ([s|^cabal-.*$|] :: ByteString))
  vs <- forM bins $ \f -> case fmap version (fmap decUTF8Safe . B.stripPrefix "cabal-" . toFilePath $ f) of
    Just (Right r) -> pure $ Right r
    Just (Left  _) -> pure $ Left f
    Nothing        -> pure $ Left f
  cs <- cabalSet -- for legacy cabal
  pure $ maybe vs (\x -> Right x:vs) cs


cabalInstalled :: Version -> IO Bool
cabalInstalled ver = do
  vers <- fmap rights $ getInstalledCabals
  pure $ elem ver $ vers


cabalSet :: (MonadIO m, MonadThrow m) => m (Maybe Version)
cabalSet = do
  cabalbin <- (</> [rel|cabal|]) <$> liftIO ghcupBinDir
  mc       <- liftIO $ handleIO (\_ -> pure Nothing) $ fmap Just $ executeOut
    cabalbin
    ["--numeric-version"]
    Nothing
  fmap join $ forM mc $ \c -> if
             | not (B.null (_stdOut c))
             , _exitCode c == ExitSuccess -> do
                  let reportedVer = fst . B.spanEnd (== _lf) . _stdOut $ c
                  case version $ decUTF8Safe reportedVer of
                    Left  e -> throwM e
                    Right r -> pure $ Just r
             | otherwise -> pure Nothing



    -----------------------------------------
    --[ Major version introspection (X.Y) ]--
    -----------------------------------------


getMajorMinorV :: MonadThrow m => Version -> m (Int, Int)
getMajorMinorV Version {..} = case _vChunks of
  ([Digits x] : [Digits y] : _) -> pure (fromIntegral x, fromIntegral y)
  _ -> throwM $ ParseError "Could not parse X.Y from version"


matchMajor :: Version -> Int -> Int -> Bool
matchMajor v' major' minor' = case getMajorMinorV v' of
  Just (x, y) -> x == major' && y == minor'
  Nothing     -> False


-- | Get the latest installed full GHC version that satisfies X.Y.
-- This reads `ghcupGHCBaseDir`.
getGHCForMajor :: (MonadIO m, MonadThrow m)
               => Int        -- ^ major version component
               -> Int        -- ^ minor version component
               -> Maybe Text -- ^ the target triple
               -> m (Maybe GHCTargetVersion)
getGHCForMajor major' minor' mt = do
  ghcs <- rights <$> getInstalledGHCs

  pure
    . lastMay
    . sortBy (\x y -> compare (_tvVersion x) (_tvVersion y))
    . filter
        (\GHCTargetVersion {..} ->
          _tvTarget == mt && matchMajor _tvVersion major' minor'
        )
    $ ghcs


-- | Get the latest available ghc for X.Y major version.
getLatestGHCFor :: Int -- ^ major version component
                -> Int -- ^ minor version component
                -> GHCupDownloads
                -> Maybe Version
getLatestGHCFor major' minor' dls = do
  join
    . fmap (lastMay . filter (\v -> matchMajor v major' minor'))
    . preview (ix GHC % to Map.keys)
    $ dls





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
getTagged :: Tag
          -> AffineFold (Map.Map Version VersionInfo) (Version, VersionInfo)
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
getLatestBaseVersion av pvpVer =
  headOf (ix GHC % getTagged (Base pvpVer) % to fst) av



    -----------------------
    --[ Settings Getter ]--
    -----------------------


getCache :: MonadReader Settings m => m Bool
getCache = ask <&> cache


getDownloader :: MonadReader Settings m => m Downloader
getDownloader = ask <&> downloader



    -------------
    --[ Other ]--
    -------------


urlBaseName :: MonadThrow m
            => ByteString  -- ^ the url path (without scheme and host)
            -> m (Path Rel)
urlBaseName = parseRel . snd . B.breakEnd (== _slash) . urlDecode False


-- Get tool files from '~/.ghcup/bin/ghc/<ver>/bin/*'
-- while ignoring *-<ver> symlinks and accounting for cross triple prefix.
--
-- Returns unversioned relative files, e.g.:
--   ["hsc2hs","haddock","hpc","runhaskell","ghc","ghc-pkg","ghci","runghc","hp2ps"]
ghcToolFiles :: (MonadThrow m, MonadFail m, MonadIO m)
             => GHCTargetVersion
             -> Excepts '[NotInstalled] m [Path Rel]
ghcToolFiles ver = do
  ghcdir <- liftIO $ ghcupGHCDir ver
  let bindir = ghcdir </> [rel|bin|]

  -- fail if ghc is not installed
  whenM (fmap not $ liftIO $ doesDirectoryExist ghcdir)
        (throwE (NotInstalled GHC (prettyTVer ver)))

  files    <- liftIO $ getDirsFiles' bindir
  -- figure out the <ver> suffix, because this might not be `Version` for
  -- alpha/rc releases, but x.y.a.somedate.

  -- for cross, this won't be "ghc", but e.g.
  -- "armv7-unknown-linux-gnueabihf-ghc"
  [ghcbin] <- liftIO $ findFiles
    bindir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^([a-zA-Z0-9_-]*[a-zA-Z0-9_]-)?ghc$|] :: ByteString)
    )

  let ghcbinPath = toFilePath (bindir </> ghcbin)
  ghcIsHadrian <- liftIO $ isHadrian ghcbinPath
  onlyUnversioned <- if ghcIsHadrian
    then pure id
    else do
      (Just symver) <-
        (B.stripPrefix (toFilePath ghcbin <> "-") . takeFileName)
          <$> (liftIO $ readSymbolicLink ghcbinPath)
      when (B.null symver)
           (throwIO $ userError $ "Fatal: ghc symlink target is broken")
      pure $ filter (\x -> not $ symver `B.isSuffixOf` toFilePath x)

  pure $ onlyUnversioned files
  where
    -- GHC is moving some builds to Hadrian for bindists, which doesn't create versioned binaries
    -- https://gitlab.haskell.org/haskell/ghcup-hs/issues/31
    isHadrian :: ByteString -> IO Bool
    isHadrian = (not . isSymbolicLink <$>) . getFileStatus


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
getChangeLog dls tool (Right tag) =
  preview (ix tool % getTagged tag % to snd % viChangeLog % _Just) dls


-- | Execute a build action while potentially cleaning up:
--
--   1. the build directory, depending on the KeepDirs setting
--   2. the install destination, depending on whether the build failed
runBuildAction :: (Show (V e), MonadReader Settings m, MonadIO m, MonadMask m)
               => Path Abs          -- ^ build directory
               -> Maybe (Path Abs)  -- ^ install location (e.g. for GHC)
               -> Excepts e m a
               -> Excepts '[BuildFailed] m a
runBuildAction bdir instdir action = do
  Settings {..} <- lift ask
  v <- flip
      onException
      (do
        forM_ instdir $ \dir ->
          liftIO $ hideError doesNotExistErrorType $ deleteDirRecursive dir
        when (keepDirs == Never)
          $ liftIO
          $ hideError doesNotExistErrorType
          $ deleteDirRecursive bdir
      )
    $ catchAllE
        (\es -> do
          forM_ instdir $ \dir ->
            liftIO $ hideError doesNotExistErrorType $ deleteDirRecursive dir
          when (keepDirs == Never)
            $ liftIO
            $ hideError doesNotExistErrorType
            $ deleteDirRecursive bdir
          throwE (BuildFailed bdir es)
        )
    $ action

  when (keepDirs == Never || keepDirs == Errors) $ liftIO $ deleteDirRecursive
    bdir
  pure v
