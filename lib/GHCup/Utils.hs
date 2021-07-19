{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

{-|
Module      : GHCup.Utils
Description : GHCup domain specific utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains GHCup helpers specific to
installation and introspection of files/versions etc.
-}
module GHCup.Utils
  ( module GHCup.Utils.Dirs
  , module GHCup.Utils
  )
where


#if defined(IS_WINDOWS)
import           GHCup.Download
#endif
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Utils.File
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

#if !defined(TAR)
import           Codec.Archive           hiding ( Directory )
#endif
import           Codec.Archive.Zip
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
#if defined(IS_WINDOWS)
import           Data.Bits
#endif
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.Foldable
import           Data.List
import           Data.List.Extra
import           Data.List.NonEmpty             ( NonEmpty( (:|) ))
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Safe
import           System.Directory      hiding   ( findFiles )
import           System.FilePath
import           System.IO.Error
#if defined(IS_WINDOWS)
import           System.Win32.Console
import           System.Win32.File     hiding ( copyFile )
import           System.Win32.Types
#endif
import           Text.Regex.Posix
import           URI.ByteString

#if defined(TAR)
import qualified Codec.Archive.Tar             as Tar
#endif
import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP





    ------------------------
    --[ Symlink handling ]--
    ------------------------


-- | The symlink destination of a ghc tool.
ghcLinkDestination :: ( MonadReader env m
                      , HasDirs env
                      , MonadThrow m, MonadIO m)
                   => FilePath -- ^ the tool, such as 'ghc', 'haddock' etc.
                   -> GHCTargetVersion
                   -> m FilePath
ghcLinkDestination tool ver = do
  Dirs {..}  <- getDirs
  ghcd <- ghcupGHCDir ver
  pure (relativeSymlink binDir (ghcd </> "bin" </> tool))


-- | Removes the minor GHC symlinks, e.g. ghc-8.6.5.
rmMinorSymlinks :: ( MonadReader env m
                   , HasDirs env
                   , MonadIO m
                   , MonadLogger m
                   , MonadThrow m
                   , MonadFail m
                   )
                => GHCTargetVersion
                -> Excepts '[NotInstalled] m ()
rmMinorSymlinks tv@GHCTargetVersion{..} = do
  Dirs {..}  <- lift getDirs

  files                         <- liftE $ ghcToolFiles tv
  forM_ files $ \f -> do
    let f_xyz = f <> "-" <> T.unpack (prettyVer _tvVersion) <> exeExt
    let fullF = binDir </> f_xyz
    lift $ $(logDebug) [i|rm -f #{fullF}|]
    liftIO $ hideError doesNotExistErrorType $ rmLink fullF


-- | Removes the set ghc version for the given target, if any.
rmPlain :: ( MonadReader env m
           , HasDirs env
           , MonadLogger m
           , MonadThrow m
           , MonadFail m
           , MonadIO m
           )
        => Maybe Text -- ^ target
        -> Excepts '[NotInstalled] m ()
rmPlain target = do
  Dirs {..}  <- lift getDirs
  mtv                           <- lift $ ghcSet target
  forM_ mtv $ \tv -> do
    files <- liftE $ ghcToolFiles tv
    forM_ files $ \f -> do
      let fullF = binDir </> f <> exeExt
      lift $ $(logDebug) [i|rm -f #{fullF}|]
      liftIO $ hideError doesNotExistErrorType $ rmLink fullF
    -- old ghcup
    let hdc_file = binDir </> "haddock-ghc" <> exeExt
    lift $ $(logDebug) [i|rm -f #{hdc_file}|]
    liftIO $ hideError doesNotExistErrorType $ rmLink hdc_file


-- | Remove the major GHC symlink, e.g. ghc-8.6.
rmMajorSymlinks :: ( MonadReader env m
                   , HasDirs env
                   , MonadIO m
                   , MonadLogger m
                   , MonadThrow m
                   , MonadFail m
                   )
                => GHCTargetVersion
                -> Excepts '[NotInstalled] m ()
rmMajorSymlinks tv@GHCTargetVersion{..} = do
  Dirs {..}  <- lift getDirs
  (mj, mi) <- getMajorMinorV _tvVersion
  let v' = intToText mj <> "." <> intToText mi

  files                         <- liftE $ ghcToolFiles tv
  forM_ files $ \f -> do
    let f_xy = f <> "-" <> T.unpack v' <> exeExt
    let fullF = binDir </> f_xy
    lift $ $(logDebug) [i|rm -f #{fullF}|]
    liftIO $ hideError doesNotExistErrorType $ rmLink fullF




    -----------------------------------
    --[ Set/Installed introspection ]--
    -----------------------------------


-- | Whether the given GHC versin is installed.
ghcInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadThrow m) => GHCTargetVersion -> m Bool
ghcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  liftIO $ doesDirectoryExist ghcdir


-- | Whether the given GHC version is installed from source.
ghcSrcInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadThrow m) => GHCTargetVersion -> m Bool
ghcSrcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  liftIO $ doesFileExist (ghcdir </> ghcUpSrcBuiltFile)


-- | Whether the given GHC version is set as the current.
ghcSet :: (MonadReader env m, HasDirs env, MonadThrow m, MonadIO m)
       => Maybe Text   -- ^ the target of the GHC version, if any
                       --  (e.g. armv7-unknown-linux-gnueabihf)
       -> m (Maybe GHCTargetVersion)
ghcSet mtarget = do
  Dirs {..}  <- getDirs
  let ghc = maybe "ghc" (\t -> T.unpack t <> "-ghc") mtarget
  let ghcBin = binDir </> ghc <> exeExt

  -- link destination is of the form ../ghc/<ver>/bin/ghc
  -- for old ghcup, it is ../ghc/<ver>/bin/ghc-<ver>
  liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    link <- liftIO $ getLinkTarget ghcBin
    Just <$> ghcLinkVersion link
 where
  ghcLinkVersion :: MonadThrow m => FilePath -> m GHCTargetVersion
  ghcLinkVersion (T.pack . dropSuffix exeExt -> t) = throwEither $ MP.parse parser "ghcLinkVersion" t
   where
    parser =
        (do
           _    <- parseUntil1 ghcSubPath
           _    <- ghcSubPath
           r    <- parseUntil1 pathSep
           rest <- MP.getInput
           MP.setInput r
           x <- ghcTargetVerP
           MP.setInput rest
           pure x
         )
        <* pathSep
        <* MP.takeRest
        <* MP.eof
    ghcSubPath = pathSep <* MP.chunk "ghc" *> pathSep

-- | Get all installed GHCs by reading ~/.ghcup/ghc/<dir>.
-- If a dir cannot be parsed, returns left.
getInstalledGHCs :: (MonadReader env m, HasDirs env, MonadIO m) => m [Either FilePath GHCTargetVersion]
getInstalledGHCs = do
  ghcdir <- ghcupGHCBaseDir
  fs     <- liftIO $ hideErrorDef [NoSuchThing] [] $ listDirectory ghcdir
  forM fs $ \f -> case parseGHCupGHCDir f of
    Right r -> pure $ Right r
    Left  _ -> pure $ Left f


-- | Get all installed cabals, by matching on @~\/.ghcup\/bin/cabal-*@.
getInstalledCabals :: ( MonadLogger m
                      , MonadReader env m
                      , HasDirs env
                      , MonadIO m
                      , MonadCatch m
                      )
                   => m [Either FilePath Version]
getInstalledCabals = do
  Dirs {..} <- getDirs
  bins   <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended execBlank ([s|^cabal-.*$|] :: ByteString))
  vs <- forM bins $ \f -> case version . T.pack <$> (stripSuffix exeExt =<< stripPrefix "cabal-" f) of
    Just (Right r) -> pure $ Right r
    Just (Left  _) -> pure $ Left f
    Nothing        -> pure $ Left f
  pure $ nub vs


-- | Whether the given cabal version is installed.
cabalInstalled :: (MonadLogger m, MonadIO m, MonadReader env m, HasDirs env, MonadCatch m) => Version -> m Bool
cabalInstalled ver = do
  vers <- fmap rights getInstalledCabals
  pure $ elem ver vers


-- Return the currently set cabal version, if any.
cabalSet :: (MonadLogger m, MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m) => m (Maybe Version)
cabalSet = do
  Dirs {..}  <- getDirs
  let cabalbin = binDir </> "cabal" <> exeExt

  handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- liftIO $ isBrokenSymlink cabalbin
    if broken
      then pure Nothing
      else do
        link <- liftIO
          $ handleIO' InvalidArgument
            (\e -> pure $ Left (toException e))
          $ fmap Right $ getLinkTarget cabalbin
        case linkVersion =<< link of
          Right v -> pure $ Just v
          Left err -> do
            $(logWarn) [i|Failed to parse cabal symlink target with: "#{err}". The symlink #{cabalbin} needs to point to valid cabal binary, such as 'cabal-3.4.0.0'.|]
            pure Nothing
 where
  -- We try to be extra permissive with link destination parsing,
  -- because of:
  --   https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/119
  linkVersion :: MonadThrow m => FilePath -> m Version
  linkVersion = throwEither . MP.parse parser "linkVersion" . T.pack . dropSuffix exeExt

  parser
    =   MP.try (stripAbsolutePath *> cabalParse)
    <|> MP.try (stripRelativePath *> cabalParse)
    <|> cabalParse
  -- parses the version of "cabal-3.2.0.0" -> "3.2.0.0"
  cabalParse = MP.chunk "cabal-" *> version'
  -- parses any path component ending with path separator,
  -- e.g. "foo/"
  stripPathComponet = parseUntil1 pathSep *> pathSep
  -- parses an absolute path up until the last path separator,
  -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
  stripAbsolutePath = pathSep *> MP.many (MP.try stripPathComponet)
  -- parses a relative path up until the last path separator,
  -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
  stripRelativePath = MP.many (MP.try stripPathComponet)



-- | Get all installed hls, by matching on
-- @~\/.ghcup\/bin/haskell-language-server-wrapper-<\hlsver\>@.
getInstalledHLSs :: (MonadReader env m, HasDirs env, MonadIO m, MonadCatch m)
                 => m [Either FilePath Version]
getInstalledHLSs = do
  Dirs {..}  <- getDirs
  bins                          <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-wrapper-.*$|] :: ByteString)
    )
  forM bins $ \f ->
    case
          version . T.pack <$> (stripSuffix exeExt =<< stripPrefix "haskell-language-server-wrapper-" f)
      of
        Just (Right r) -> pure $ Right r
        Just (Left  _) -> pure $ Left f
        Nothing        -> pure $ Left f

-- | Get all installed stacks, by matching on
-- @~\/.ghcup\/bin/stack-<\stackver\>@.
getInstalledStacks :: (MonadReader env m, HasDirs env, MonadIO m, MonadCatch m)
                   => m [Either FilePath Version]
getInstalledStacks = do
  Dirs {..}  <- getDirs
  bins                          <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^stack-.*$|] :: ByteString)
    )
  forM bins $ \f ->
    case version . T.pack <$> (stripSuffix exeExt =<< stripPrefix "stack-" f) of
        Just (Right r) -> pure $ Right r
        Just (Left  _) -> pure $ Left f
        Nothing        -> pure $ Left f

-- Return the currently set stack version, if any.
-- TODO: there's a lot of code duplication here :>
stackSet :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m) => m (Maybe Version)
stackSet = do
  Dirs {..}  <- getDirs
  let stackBin = binDir </> "stack" <> exeExt

  handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- liftIO $ isBrokenSymlink stackBin
    if broken
      then pure Nothing
      else do
        link <- liftIO
          $ handleIO' InvalidArgument
            (\e -> pure $ Left (toException e))
          $ fmap Right $ getLinkTarget stackBin
        case linkVersion =<< link of
          Right v -> pure $ Just v
          Left err -> do
            $(logWarn) [i|Failed to parse stack symlink target with: "#{err}". The symlink #{stackBin} needs to point to valid stack binary, such as 'stack-2.7.1'.|]
            pure Nothing
 where
  linkVersion :: MonadThrow m => FilePath -> m Version
  linkVersion = throwEither . MP.parse parser "" . T.pack . dropSuffix exeExt
   where
    parser
      =   MP.try (stripAbsolutePath *> cabalParse)
      <|> MP.try (stripRelativePath *> cabalParse)
      <|> cabalParse
    -- parses the version of "stack-2.7.1" -> "2.7.1"
    cabalParse = MP.chunk "stack-" *> version'
    -- parses any path component ending with path separator,
    -- e.g. "foo/"
    stripPathComponet = parseUntil1 pathSep *> pathSep
    -- parses an absolute path up until the last path separator,
    -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
    stripAbsolutePath = pathSep *> MP.many (MP.try stripPathComponet)
    -- parses a relative path up until the last path separator,
    -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
    stripRelativePath = MP.many (MP.try stripPathComponet)

-- | Whether the given Stack version is installed.
stackInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadCatch m) => Version -> m Bool
stackInstalled ver = do
  vers <- fmap rights getInstalledStacks
  pure $ elem ver vers

-- | Whether the given HLS version is installed.
hlsInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadCatch m) => Version -> m Bool
hlsInstalled ver = do
  vers <- fmap rights getInstalledHLSs
  pure $ elem ver vers



-- Return the currently set hls version, if any.
hlsSet :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m) => m (Maybe Version)
hlsSet = do
  Dirs {..}  <- getDirs
  let hlsBin = binDir </> "haskell-language-server-wrapper" <> exeExt

  liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- isBrokenSymlink hlsBin
    if broken
      then pure Nothing
      else do
        link <- liftIO $ getLinkTarget hlsBin
        Just <$> linkVersion link
 where
  linkVersion :: MonadThrow m => FilePath -> m Version
  linkVersion = throwEither . MP.parse parser "" . T.pack . dropSuffix exeExt
   where
    parser
      =   MP.try (stripAbsolutePath *> cabalParse)
      <|> MP.try (stripRelativePath *> cabalParse)
      <|> cabalParse
    -- parses the version of "haskell-language-server-wrapper-1.1.0" -> "1.1.0"
    cabalParse = MP.chunk "haskell-language-server-wrapper-" *> version'
    -- parses any path component ending with path separator,
    -- e.g. "foo/"
    stripPathComponet = parseUntil1 pathSep *> pathSep
    -- parses an absolute path up until the last path separator,
    -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
    stripAbsolutePath = pathSep *> MP.many (MP.try stripPathComponet)
    -- parses a relative path up until the last path separator,
    -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
    stripRelativePath = MP.many (MP.try stripPathComponet)


-- | Return the GHC versions the currently selected HLS supports.
hlsGHCVersions :: ( MonadReader env m
                  , HasDirs env
                  , MonadIO m
                  , MonadThrow m
                  , MonadCatch m
                  )
               => m [Version]
hlsGHCVersions = do
  h                             <- hlsSet
  vers                          <- forM h $ \h' -> do
    bins <- hlsServerBinaries h'
    pure $ fmap
      (version
        . T.pack
        . fromJust
        . stripPrefix "haskell-language-server-"
        . head
        . splitOn "~"
        )
      bins
  pure . rights . concat . maybeToList $ vers


-- | Get all server binaries for an hls version, if any.
hlsServerBinaries :: (MonadReader env m, HasDirs env, MonadIO m)
                  => Version
                  -> m [FilePath]
hlsServerBinaries ver = do
  Dirs {..}  <- getDirs
  liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts
      compExtended
      execBlank
      ([s|^haskell-language-server-.*~|] <> escapeVerRex ver <> E.encodeUtf8 (T.pack exeExt) <> [s|$|] :: ByteString
      )
    )


-- | Get the wrapper binary for an hls version, if any.
hlsWrapperBinary :: (MonadReader env m, HasDirs env, MonadThrow m, MonadIO m)
                 => Version
                 -> m (Maybe FilePath)
hlsWrapperBinary ver = do
  Dirs {..}  <- getDirs
  wrapper <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts
      compExtended
      execBlank
      ([s|^haskell-language-server-wrapper-|] <> escapeVerRex ver <> E.encodeUtf8 (T.pack exeExt) <> [s|$|] :: ByteString
      )
    )
  case wrapper of
    []  -> pure Nothing
    [x] -> pure $ Just x
    _   -> throwM $ UnexpectedListLength
      "There were multiple hls wrapper binaries for a single version"


-- | Get all binaries for an hls version, if any.
hlsAllBinaries :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m) => Version -> m [FilePath]
hlsAllBinaries ver = do
  hls     <- hlsServerBinaries ver
  wrapper <- hlsWrapperBinary ver
  pure (maybeToList wrapper ++ hls)


-- | Get the active symlinks for hls.
hlsSymlinks :: (MonadReader env m, HasDirs env, MonadIO m, MonadCatch m) => m [FilePath]
hlsSymlinks = do
  Dirs {..}  <- getDirs
  oldSyms                       <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-.*$|] :: ByteString)
    )
  filterM
    ( liftIO
    . pathIsLink
    . (binDir </>)
    )
    oldSyms



    -----------------------------------------
    --[ Major version introspection (X.Y) ]--
    -----------------------------------------


-- | Extract (major, minor) from any version.
getMajorMinorV :: MonadThrow m => Version -> m (Int, Int)
getMajorMinorV Version {..} = case _vChunks of
  ((Digits x :| []) :| ((Digits y :| []):_)) -> pure (fromIntegral x, fromIntegral y)
  _ -> throwM $ ParseError "Could not parse X.Y from version"


matchMajor :: Version -> Int -> Int -> Bool
matchMajor v' major' minor' = case getMajorMinorV v' of
  Just (x, y) -> x == major' && y == minor'
  Nothing     -> False


-- | Get the latest installed full GHC version that satisfies X.Y.
-- This reads `ghcupGHCBaseDir`.
getGHCForMajor :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m)
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
                -> Maybe (Version, VersionInfo)
getLatestGHCFor major' minor' dls =
  preview (ix GHC % to Map.toDescList) dls >>= lastMay . filter (\(v, _) -> matchMajor v major' minor')




    -----------------
    --[ Unpacking ]--
    -----------------



-- | Unpack an archive to a temporary directory and return that path.
unpackToDir :: (MonadLogger m, MonadIO m, MonadThrow m)
            => FilePath       -- ^ destination dir
            -> FilePath       -- ^ archive path
            -> Excepts '[UnknownArchive
#if !defined(TAR)
                        , ArchiveResult
#endif
                        ] m ()
unpackToDir dfp av = do
  let fn = takeFileName av
  lift $ $(logInfo) [i|Unpacking: #{fn} to #{dfp}|]

#if defined(TAR)
  let untar :: MonadIO m => BL.ByteString -> Excepts '[] m ()
      untar = liftIO . Tar.unpack dfp . Tar.read

      rf :: MonadIO m => FilePath -> Excepts '[] m BL.ByteString
      rf = liftIO . BL.readFile
#else
  let untar :: MonadIO m => BL.ByteString -> Excepts '[ArchiveResult] m ()
      untar = lEM . liftIO . runArchiveM . unpackToDirLazy dfp

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#endif

  -- extract, depending on file extension
  if
    | ".tar.gz" `isSuffixOf` fn -> liftE
      (untar . GZip.decompress =<< rf av)
    | ".tar.xz" `isSuffixOf` fn -> do
      filecontents <- liftE $ rf av
      let decompressed = Lzma.decompress filecontents
      liftE $ untar decompressed
    | ".tar.bz2" `isSuffixOf` fn ->
      liftE (untar . BZip.decompress =<< rf av)
    | ".tar" `isSuffixOf` fn -> liftE (untar =<< rf av)
    | ".zip" `isSuffixOf` fn ->
      withArchive av (unpackInto dfp)
    | otherwise -> throwE $ UnknownArchive fn


getArchiveFiles :: (MonadLogger m, MonadIO m, MonadThrow m)
                => FilePath       -- ^ archive path
                -> Excepts '[UnknownArchive
#if defined(TAR)
                            , Tar.FormatError
#else
                            , ArchiveResult
#endif
                            ] m [FilePath]
getArchiveFiles av = do
  let fn = takeFileName av

#if defined(TAR)
  let entries :: Monad m => BL.ByteString -> Excepts '[Tar.FormatError] m [FilePath]
      entries =
          lE @Tar.FormatError
          . Tar.foldEntries
            (\e x -> fmap (Tar.entryPath e :) x)
            (Right [])
            (\e -> Left e)
          . Tar.read

      rf :: MonadIO m => FilePath -> Excepts '[Tar.FormatError] m BL.ByteString
      rf = liftIO . BL.readFile
#else
  let entries :: Monad m => BL.ByteString -> Excepts '[ArchiveResult] m [FilePath]
      entries = (fmap . fmap) filepath . lE . readArchiveBSL

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile
#endif

  -- extract, depending on file extension
  if
    | ".tar.gz" `isSuffixOf` fn -> liftE
      (entries . GZip.decompress =<< rf av)
    | ".tar.xz" `isSuffixOf` fn -> do
      filecontents <- liftE $ rf av
      let decompressed = Lzma.decompress filecontents
      liftE $ entries decompressed
    | ".tar.bz2" `isSuffixOf` fn ->
      liftE (entries . BZip.decompress =<< rf av)
    | ".tar" `isSuffixOf` fn -> liftE (entries =<< rf av)
    | ".zip" `isSuffixOf` fn ->
      withArchive av $ do
        entries' <- getEntries
        pure $ fmap unEntrySelector $ Map.keys entries'
    | otherwise -> throwE $ UnknownArchive fn


intoSubdir :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
           => FilePath       -- ^ unpacked tar dir
           -> TarDir         -- ^ how to descend
           -> Excepts '[TarDirDoesNotExist] m FilePath
intoSubdir bdir tardir = case tardir of
  RealDir pr -> do
    whenM (fmap not . liftIO . doesDirectoryExist $ (bdir </> pr))
          (throwE $ TarDirDoesNotExist tardir)
    pure (bdir </> pr)
  RegexDir r -> do
    let rs = split (`elem` pathSeparators) r
    foldlM
      (\y x ->
        (handleIO (\_ -> pure []) . liftIO . findFiles y . regex $ x) >>= (\case
          []      -> throwE $ TarDirDoesNotExist tardir
          (p : _) -> pure (y </> p)) . sort
      )
      bdir
      rs
    where regex = makeRegexOpts compIgnoreCase execBlank




    ------------
    --[ Tags ]--
    ------------


-- | Get the tool version that has this tag. If multiple have it,
-- picks the greatest version.
getTagged :: Tag
          -> AffineFold (Map.Map Version VersionInfo) (Version, VersionInfo)
getTagged tag =
  to (Map.filter (\VersionInfo {..} -> tag `elem` _viTags))
  % to Map.toDescList
  % _head

getLatest :: GHCupDownloads -> Tool -> Maybe (Version, VersionInfo)
getLatest av tool = headOf (ix tool % getTagged Latest) av

getRecommended :: GHCupDownloads -> Tool -> Maybe (Version, VersionInfo)
getRecommended av tool = headOf (ix tool % getTagged Recommended) av


-- | Gets the latest GHC with a given base version.
getLatestBaseVersion :: GHCupDownloads -> PVP -> Maybe (Version, VersionInfo)
getLatestBaseVersion av pvpVer =
  headOf (ix GHC % getTagged (Base pvpVer)) av




    -------------
    --[ Other ]--
    -------------


-- | Get tool files from @~\/.ghcup\/bin\/ghc\/\<ver\>\/bin\/\*@
-- while ignoring @*-\<ver\>@ symlinks and accounting for cross triple prefix.
--
-- Returns unversioned relative files without extension, e.g.:
--
--   - @["hsc2hs","haddock","hpc","runhaskell","ghc","ghc-pkg","ghci","runghc","hp2ps"]@
ghcToolFiles :: (MonadReader env m, HasDirs env, MonadThrow m, MonadFail m, MonadIO m)
             => GHCTargetVersion
             -> Excepts '[NotInstalled] m [FilePath]
ghcToolFiles ver = do
  ghcdir <- lift $ ghcupGHCDir ver
  let bindir = ghcdir </> "bin"

  -- fail if ghc is not installed
  whenM (fmap not $ liftIO $ doesDirectoryExist ghcdir)
        (throwE (NotInstalled GHC ver))

  files    <- liftIO $ listDirectory bindir
  -- figure out the <ver> suffix, because this might not be `Version` for
  -- alpha/rc releases, but x.y.a.somedate.

  ghcIsHadrian    <- liftIO $ isHadrian bindir
  onlyUnversioned <- case ghcIsHadrian of
    Right () -> pure id
    Left (fmap (dropSuffix exeExt) -> [ghc, ghc_ver])
      | (Just symver) <- stripPrefix (ghc <> "-") ghc_ver
      , not (null symver) -> pure $ filter (\x -> not $ symver `isInfixOf` x)
    _ -> fail "Fatal: Could not find internal GHC version"

  pure $ onlyUnversioned $ fmap (dropSuffix exeExt) files
 where
  isNotAnyInfix xs t = foldr (\a b -> not (a `isInfixOf` t) && b) True xs
    -- GHC is moving some builds to Hadrian for bindists,
    -- which doesn't create versioned binaries.
    -- https://gitlab.haskell.org/haskell/ghcup-hs/issues/31
  isHadrian :: FilePath -- ^ ghcbin path
            -> IO (Either [String] ()) -- ^ Right for Hadrian
  isHadrian dir = do
    -- Non-hadrian has e.g. ["ghc", "ghc-8.10.4"]
    -- which also requires us to discover the internal version
    -- to filter the correct tool files.
    -- We can't use the symlink on windows, so we fall back to some
    -- more complicated logic.
    fs <- fmap
         -- regex over-matches
         (filter (isNotAnyInfix ["haddock", "ghc-pkg", "ghci"]))
       $ liftIO $ findFiles
      dir
      (makeRegexOpts compExtended
                     execBlank
                     -- for cross, this won't be "ghc", but e.g.
                     -- "armv7-unknown-linux-gnueabihf-ghc"
                     ([s|^([a-zA-Z0-9_-]*[a-zA-Z0-9_]-)?ghc.*$|] :: ByteString)
      )
    if | length fs == 1 -> pure $ Right ()        -- hadrian
       | length fs == 2 -> pure $ Left
                              (sortOn length fs)  -- legacy make, result should
                                                  -- be ["ghc", "ghc-8.10.4"]
       | otherwise      -> fail "isHadrian failed!"



-- | This file, when residing in @~\/.ghcup\/ghc\/\<ver\>\/@ signals that
-- this GHC was built from source. It contains the build config.
ghcUpSrcBuiltFile :: FilePath
ghcUpSrcBuiltFile = ".ghcup_src_built"


-- | Calls gmake if it exists in PATH, otherwise make.
make :: ( MonadThrow m
        , MonadIO m
        , MonadReader env m
        , HasDirs env
        , HasSettings env
        )
     => [String]
     -> Maybe FilePath
     -> m (Either ProcessError ())
make args workdir = do
  spaths    <- liftIO getSearchPath
  has_gmake <- isJust <$> liftIO (searchPath spaths "gmake")
  let mymake = if has_gmake then "gmake" else "make"
  execLogged mymake args workdir "ghc-make" Nothing

makeOut :: (MonadReader env m, HasDirs env, MonadIO m)
        => [String]
        -> Maybe FilePath
        -> m CapturedProcess
makeOut args workdir = do
  spaths    <- liftIO getSearchPath
  has_gmake <- isJust <$> liftIO (searchPath spaths "gmake")
  let mymake = if has_gmake then "gmake" else "make"
  executeOut mymake args workdir


-- | Try to apply patches in order. Fails with 'PatchFailed'
-- on first failure.
applyPatches :: (MonadReader env m, HasDirs env, MonadLogger m, MonadIO m)
             => FilePath   -- ^ dir containing patches
             -> FilePath   -- ^ dir to apply patches in
             -> Excepts '[PatchFailed] m ()
applyPatches pdir ddir = do
  patches <- (fmap . fmap) (pdir </>) $ liftIO $ listDirectory pdir
  forM_ (sort patches) $ \patch' -> do
    lift $ $(logInfo) [i|Applying patch #{patch'}|]
    fmap (either (const Nothing) Just)
         (exec
           "patch"
           ["-p1", "-i", patch']
           (Just ddir)
           Nothing)
      !? PatchFailed


-- | https://gitlab.haskell.org/ghc/ghc/-/issues/17353
darwinNotarization :: (MonadReader env m, HasDirs env, MonadIO m)
                   => Platform
                   -> FilePath
                   -> m (Either ProcessError ())
darwinNotarization Darwin path = exec
  "xattr"
  ["-r", "-d", "com.apple.quarantine", path]
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
runBuildAction :: (Show (V e), MonadReader env m, HasDirs env, HasSettings env, MonadIO m, MonadMask m)
               => FilePath          -- ^ build directory (cleaned up depending on Settings)
               -> Maybe FilePath  -- ^ dir to *always* clean up on exception
               -> Excepts e m a
               -> Excepts '[BuildFailed] m a
runBuildAction bdir instdir action = do
  Settings {..} <- lift getSettings
  let exAction = do
        forM_ instdir $ \dir ->
          liftIO $ hideError doesNotExistErrorType $ rmPath dir
        when (keepDirs == Never)
          $ liftIO
          $ hideError doesNotExistErrorType
          $ rmPath bdir
  v <-
    flip onException exAction
    $ catchAllE
        (\es -> do
          exAction
          throwE (BuildFailed bdir es)
        ) action

  when (keepDirs == Never || keepDirs == Errors) $ liftIO $ rmPath bdir
  pure v


getVersionInfo :: Version
               -> Tool
               -> GHCupDownloads
               -> Maybe VersionInfo
getVersionInfo v' tool =
  headOf
    ( ix tool
    % to (Map.filterWithKey (\k _ -> k == v'))
    % to Map.elems
    % _head
    )


-- | The file extension for executables.
exeExt :: String
#if defined(IS_WINDOWS)
exeExt = ".exe"
#else
exeExt = ""
#endif

-- | The file extension for executables.
exeExt' :: ByteString
#if defined(IS_WINDOWS)
exeExt' = ".exe"
#else
exeExt' = ""
#endif


-- | Enables ANSI support on windows, does nothing on unix.
--
-- Returns 'Left str' on errors and 'Right bool' on success, where
-- 'bool' markes whether ansi support was already enabled.
--
-- This function never crashes.
--
-- Rip-off of https://docs.rs/ansi_term/0.12.1/x86_64-pc-windows-msvc/src/ansi_term/windows.rs.html#10-61
enableAnsiSupport :: IO (Either String Bool)
#if defined(IS_WINDOWS)
enableAnsiSupport = handleIO (pure . Left . displayException) $ do
  -- ref: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
  -- Using `CreateFileW("CONOUT$", ...)` to retrieve the console handle works correctly even if STDOUT and/or STDERR are redirected
  h <- createFile "CONOUT$" (gENERIC_WRITE .|. gENERIC_READ)
    fILE_SHARE_WRITE Nothing oPEN_EXISTING 0 Nothing
  when (h == iNVALID_HANDLE_VALUE ) $ fail "invalid handle value"

  -- ref: https://docs.microsoft.com/en-us/windows/console/getconsolemode
  m <- getConsoleMode h

  -- VT processing not already enabled?
  if ((m .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING) == 0)
  -- https://docs.microsoft.com/en-us/windows/console/setconsolemode
  then setConsoleMode h (m .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING)
    >> pure (Right False)
  else pure (Right True)
#else
enableAnsiSupport = pure (Right True)
#endif


-- | On unix, we can use symlinks, so we just get the
-- symbolic link target.
--
-- On windows, we have to emulate symlinks via shims,
-- see 'createLink'.
getLinkTarget :: FilePath -> IO FilePath
getLinkTarget fp = do
#if defined(IS_WINDOWS)
  content <- readFile (dropExtension fp <.> "shim")
  [p] <- pure . filter ("path = " `isPrefixOf`) . lines $ content
  pure $ stripNewline $ dropPrefix "path = " p
#else
  getSymbolicLinkTarget fp
#endif


-- | Checks whether the path is a link.
pathIsLink :: FilePath -> IO Bool
#if defined(IS_WINDOWS)
pathIsLink fp = doesPathExist (dropExtension fp <.> "shim")
#else
pathIsLink = pathIsSymbolicLink
#endif


rmLink :: FilePath -> IO ()
#if defined(IS_WINDOWS)
rmLink fp = do
  hideError doesNotExistErrorType . liftIO . rmFile $ fp
  hideError doesNotExistErrorType . liftIO . rmFile $ (dropExtension fp <.> "shim")
#else
rmLink = hideError doesNotExistErrorType . liftIO . rmFile
#endif


-- | Creates a symbolic link on unix and a fake symlink on windows for
-- executables, which:
--     1. is a shim exe
--     2. has a corresponding .shim file in the same directory that
--        contains the target
--
-- This overwrites previously existing files.
--
-- On windows, this requires that 'ensureGlobalTools' was run beforehand.
createLink :: ( MonadMask m
              , MonadThrow m
              , MonadLogger m
              , MonadIO m
              , MonadReader env m
              , HasDirs env
              , MonadUnliftIO m
              , MonadFail m
              )
           => FilePath      -- ^ path to the target executable
           -> FilePath      -- ^ path to be created
           -> m ()
createLink link exe = do
#if defined(IS_WINDOWS)
  dirs <- getDirs
  let shimGen = cacheDir dirs </> "gs.exe"

  let shim = dropExtension exe <.> "shim"
      -- For hardlinks, link needs to be absolute.
      -- If link is relative, it's relative to the target exe.
      -- Note that (</>) drops lhs when rhs is absolute.
      fullLink = takeDirectory exe </> link
      shimContents = "path = " <> fullLink

  $(logDebug) [i|rm -f #{exe}|]
  liftIO $ rmLink exe

  $(logDebug) [i|ln -s #{fullLink} #{exe}|]
  liftIO $ copyFile shimGen exe
  liftIO $ writeFile shim shimContents
#else
  $(logDebug) [i|rm -f #{exe}|]
  liftIO $ hideError doesNotExistErrorType $ rmFile exe

  $(logDebug) [i|ln -s #{link} #{exe}|]
  liftIO $ createFileLink link exe
#endif


ensureGlobalTools :: ( MonadMask m
                     , MonadThrow m
                     , MonadLogger m
                     , MonadIO m
                     , MonadReader env m
                     , HasDirs env
                     , HasSettings env
                     , HasGHCupInfo env
                     , MonadUnliftIO m
                     , MonadFail m
                     )
                  => Excepts '[DigestError , DownloadFailed, NoDownload] m ()
ensureGlobalTools = do
#if defined(IS_WINDOWS)
  (GHCupInfo _ _ gTools) <- lift getGHCupInfo
  settings <- lift getSettings
  dirs <- lift getDirs
  shimDownload <- liftE $ lE @_ @'[NoDownload]
    $ maybe (Left NoDownload) Right $ Map.lookup ShimGen gTools
  let dl = downloadCached' shimDownload (Just "gs.exe") Nothing
  void $ (\(DigestError _ _) -> do
      lift $ $(logWarn) [i|Digest doesn't match, redownloading gs.exe...|]
      lift $ $(logDebug) [i|rm -f #{shimDownload}|]
      liftIO $ hideError doesNotExistErrorType $ rmFile (cacheDir dirs </> "gs.exe")
      liftE @'[DigestError , DownloadFailed] $ dl
    ) `catchE` (liftE @'[DigestError , DownloadFailed] dl)
  pure ()
#else
  pure ()
#endif


-- | Ensure ghcup directory structure exists.
ensureDirectories :: Dirs -> IO ()
ensureDirectories dirs = do
  let Dirs
        { baseDir
        , binDir
        , cacheDir
        , logsDir
        , confDir
        } = dirs
  createDirRecursive' baseDir
  createDirRecursive' binDir
  createDirRecursive' cacheDir
  createDirRecursive' logsDir
  createDirRecursive' confDir
  pure ()


-- | For ghc without arch triple, this is:
--
--    - ghc-<ver> (e.g. ghc-8.10.4)
--
-- For ghc with arch triple:
--
--    - <triple>-ghc-<ver> (e.g. arm-linux-gnueabihf-ghc-8.10.4)
ghcBinaryName :: GHCTargetVersion -> String
ghcBinaryName (GHCTargetVersion (Just t) v') = T.unpack (t <> "-ghc-" <> prettyVer v' <> T.pack exeExt)
ghcBinaryName (GHCTargetVersion Nothing  v') = T.unpack ("ghc-" <> prettyVer v' <> T.pack exeExt)

