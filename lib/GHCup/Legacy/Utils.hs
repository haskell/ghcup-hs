{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : GHCup.Legacy.Utils
Description : GHCup domain specific legacy utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains GHCup helpers specific to
installation and introspection of files/versions etc.
-}
module GHCup.Legacy.Utils where


#if defined(IS_WINDOWS)
import GHCup.Prelude.Windows
#else
import GHCup.Prelude.Posix
#endif
import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import GHCup.Errors
import GHCup.Prelude
import GHCup.Prelude.MegaParsec
import GHCup.Prelude.String.QQ
import GHCup.Query.GHCupDirs
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.JSON
    (safeVersion, cabalBadNames)
import GHCup.Types.Optics
import qualified GHCup.Warnings as Warnings

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.ByteString                ( ByteString )
import Data.Either
import Data.List
    ( groupBy, isInfixOf, isPrefixOf, isSuffixOf, nub, sortBy, stripPrefix )
import Data.Maybe
import Data.Text                      ( Text )
import Data.Variant.Excepts
import Data.Versions                  hiding ( patch )
import GHC.IO.Exception
import Safe
import System.FilePath
import System.IO.Error
import Text.Regex.Posix

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Text.Megaparsec    as MP




    ------------------------
    --[ Symlink handling ]--
    ------------------------


-- | Create a relative symlink destination for the binary directory,
-- given a target toolpath.
binarySymLinkDestination :: ( MonadThrow m
                            , MonadIO m
                            )
                         => FilePath -- ^ binary dir
                         -> FilePath -- ^ the full toolpath
                         -> m FilePath
binarySymLinkDestination binDir toolPath = do
  toolPath' <- liftIO $ canonicalizePath toolPath
  binDir' <- liftIO $ canonicalizePath binDir
  pure (relativeSymlink binDir' toolPath')


-- | Removes the minor GHC symlinks, e.g. ghc-8.6.5.
rmMinorGHCSymlinks :: ( MonadReader env m
                      , HasDirs env
                      , MonadIO m
                      , HasLog env
                      , MonadThrow m
                      , MonadFail m
                      , MonadMask m
                      )
                   => TargetVersion
                   -> Excepts '[NotInstalled] m ()
rmMinorGHCSymlinks tv@TargetVersion{..} = do
  Dirs {..}  <- lift getDirs

  files                         <- liftE $ ghcToolFiles tv
  forM_ files $ \f -> do
    let f_xyz = f <> "-" <> T.unpack (prettyVer _tvVersion) <> exeExt
    let fullF = binDir </> f_xyz
    lift $ logDebug ("rm -f " <> T.pack fullF)
    lift $ hideError doesNotExistErrorType $ rmLink fullF


-- | Removes the set ghc version for the given target, if any.
rmPlainGHC :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadThrow m
              , MonadFail m
              , MonadIO m
              , MonadMask m
              )
           => Maybe Text -- ^ target
           -> Excepts '[NotInstalled] m ()
rmPlainGHC target = do
  Dirs {..}  <- lift getDirs
  mtv                           <- lift $ ghcSet target
  forM_ mtv $ \tv -> do
    files <- liftE $ ghcToolFiles tv
    forM_ files $ \f -> do
      let fullF = binDir </> f <> exeExt
      lift $ logDebug ("rm -f " <> T.pack fullF)
      lift $ hideError doesNotExistErrorType $ rmLink fullF
    -- old ghcup
    let hdc_file = binDir </> "haddock-ghc" <> exeExt
    lift $ logDebug ("rm -f " <> T.pack hdc_file)
    lift $ hideError doesNotExistErrorType $ rmLink hdc_file


-- | Remove the major GHC symlink, e.g. ghc-8.6.
rmMajorGHCSymlinks :: ( MonadReader env m
                      , HasDirs env
                      , MonadIO m
                      , HasLog env
                      , MonadThrow m
                      , MonadFail m
                      , MonadMask m
                      )
                   => TargetVersion
                   -> Excepts '[NotInstalled] m ()
rmMajorGHCSymlinks tv@TargetVersion{..} = do
  Dirs {..}  <- lift getDirs
  (mj, mi) <- getMajorMinorV _tvVersion
  let v' = intToText mj <> "." <> intToText mi

  files                         <- liftE $ ghcToolFiles tv
  forM_ files $ \f -> do
    let f_xy = f <> "-" <> T.unpack v' <> exeExt
    let fullF = binDir </> f_xy
    lift $ logDebug ("rm -f " <> T.pack fullF)
    lift $ hideError doesNotExistErrorType $ rmLink fullF



-- | Removes the minor HLS files, e.g. 'haskell-language-server-8.10.7~1.6.1.0'
-- and 'haskell-language-server-wrapper-1.6.1.0'.
rmMinorHLSSymlinks :: ( MonadReader env m
                      , HasDirs env
                      , MonadIO m
                      , HasLog env
                      , MonadThrow m
                      , MonadFail m
                      , MonadMask m
                      )
                   => Version
                   -> Excepts '[NotInstalled] m ()
rmMinorHLSSymlinks ver = do
  Dirs {..}  <- lift getDirs

  hlsBins <- hlsAllBinaries ver
  forM_ hlsBins $ \f -> do
    let fullF = binDir </> f
    lift $ logDebug ("rm -f " <> T.pack fullF)
    -- on unix, this may be either a file (legacy) or a symlink
    -- on windows, this is always a file... hence 'rmFile'
    -- works consistently across platforms
    lift $ rmFile fullF

-- | Removes the set HLS version, if any.
rmPlainHLS :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadThrow m
              , MonadFail m
              , MonadIO m
              , MonadMask m
              )
           => Excepts '[NotInstalled] m ()
rmPlainHLS = do
  Dirs {..}  <- lift getDirs

  -- delete 'haskell-language-server-8.10.7'
  hlsBins <- fmap (filter (\f -> not ("haskell-language-server-wrapper" `isPrefixOf` f) && ('~' `notElem` f)))
    $ liftIO $ handleIO (\_ -> pure []) $ findFiles
      binDir
      (makeRegexOpts compExtended execBlank ([s|^haskell-language-server-.*$|] :: ByteString))
  forM_ hlsBins $ \f -> do
    let fullF = binDir </> f
    lift $ logDebug ("rm -f " <> T.pack fullF)
    if isWindows
    then lift $ rmLink fullF
    else lift $ rmFile fullF

  -- 'haskell-language-server-wrapper'
  let hlswrapper = binDir </> "haskell-language-server-wrapper" <> exeExt
  lift $ logDebug ("rm -f " <> T.pack hlswrapper)
  if isWindows
  then lift $ hideError doesNotExistErrorType $ rmLink hlswrapper
  else lift $ hideError doesNotExistErrorType $ rmFile hlswrapper



    -----------------------------------
    --[ Set/Installed introspection ]--
    -----------------------------------


-- | Whether the given GHC version is installed.
ghcInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadThrow m) => TargetVersion -> m Bool
ghcInstalled ver = do
  ghcdir <- ghcupGHCDir ver
  liftIO $ doesDirectoryExist (fromGHCupPath ghcdir)


-- | Whether the given GHC version is set as the current.
ghcSet :: (MonadReader env m, HasDirs env, MonadThrow m, MonadIO m)
       => Maybe Text   -- ^ the target of the GHC version, if any
                       --  (e.g. armv7-unknown-linux-gnueabihf)
       -> m (Maybe TargetVersion)
ghcSet mtarget = do
  Dirs {..}  <- getDirs
  let ghc' = maybe "ghc" (\t -> T.unpack t <> "-ghc") mtarget
  let ghcBin = binDir </> ghc' <> exeExt

  -- link destination is of the form ../ghc/<ver>/bin/ghc
  -- for old ghcup, it is ../ghc/<ver>/bin/ghc-<ver>
  liftIO $ handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    link <- liftIO $ getLinkTarget ghcBin
    Just <$> ghcLinkVersion' link
 where
  ghcLinkVersion' :: MonadThrow m => FilePath -> m TargetVersion
  ghcLinkVersion' (T.pack . dropSuffix exeExt -> t) = throwEither $
    MP.parse (MP.try ghcVersionFromPath <|> ghcLinkVersion) "ghcLinkVersion" t

-- | Get all installed GHCs by reading ~/.ghcup/ghc/<dir>.
-- If a dir cannot be parsed, returns left.
getInstalledGHCs :: (MonadReader env m, HasDirs env, MonadIO m) => m [Either FilePath TargetVersion]
getInstalledGHCs = filter (either (const True) safeVersion) <$> do
  ghcdir <- ghcupGHCBaseDir
  fs     <- liftIO $ hideErrorDef [NoSuchThing] [] $ listDirectoryDirs (fromGHCupPath ghcdir)
  forM fs $ \f -> case parseGHCupGHCDir f of
    Right r -> pure $ Right r
    Left  _ -> pure $ Left f


-- | Get all installed cabals, by matching on @~\/.ghcup\/bin/cabal-*@.
getInstalledCabals :: ( MonadReader env m
                      , HasDirs env
                      , MonadIO m
                      , MonadCatch m
                      )
                   => m [Either FilePath Version]
getInstalledCabals = filter (either (const True) (safeVersion . mkTVer)) <$> do
  Dirs {..} <- getDirs
  bins   <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended execBlank ([s|^cabal-.*$|] :: ByteString))
  vs <- forM bins $ \f -> case version . T.pack <$> (stripSuffix exeExt =<< stripPrefix "cabal-" f) of
    Just (Right r)
      | T.unpack (prettyVer r) `notPrefixElem` cabalBadNames -> pure $ Right r
      | otherwise -> pure $ Left f
    Just (Left  _) -> pure $ Left f
    Nothing        -> pure $ Left f
  pure $ nub vs
 where
  notPrefixElem a xs = not (any (`isPrefixOf` a) xs)


-- | Whether the given cabal version is installed.
cabalInstalled :: (MonadIO m, MonadReader env m, HasDirs env, MonadCatch m) => Version -> m Bool
cabalInstalled ver = do
  vers <- fmap rights getInstalledCabals
  pure $ elem ver vers


-- Return the currently set cabal version, if any.
cabalSet :: (HasLog env, MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m) => m (Maybe Version)
cabalSet = do
  Dirs {..}  <- getDirs
  let cabalbin = binDir </> "cabal" <> exeExt

  handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- liftIO $ isBrokenSymlink cabalbin
    if broken
      then do
        logWarn $ "Broken symlink at " <> T.pack cabalbin
        pure Nothing
      else do
        link <- liftIO
          $ handleIO' InvalidArgument
            (\e -> pure $ Left (toException e))
          $ fmap Right $ getLinkTarget cabalbin
        case linkVersion =<< link of
          Right v -> pure $ Just v
          Left err -> do
            logWarn $ "Failed to parse cabal symlink target with: "
              <> T.pack (displayException err)
              <> ". The symlink "
              <> T.pack cabalbin
              <> " needs to point to valid cabal binary, such as 'cabal-3.4.0.0'."
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
    <|> (_tvVersion <$> toolVersionFromPath cabal)
  -- parses the version of "cabal-3.2.0.0" -> "3.2.0.0"
  cabalParse = MP.chunk "cabal-" *> version'
  -- parses any path component ending with path separator,
  -- e.g. "foo/"
  stripPathComponent = parseUntil1 pathSep *> MP.some pathSep
  -- parses an absolute path up until the last path separator,
  -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
  stripAbsolutePath = MP.some pathSep *> MP.many (MP.try stripPathComponent)
  -- parses a relative path up until the last path separator,
  -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
  stripRelativePath = MP.many (MP.try stripPathComponent)



-- | Get all installed hls, by matching on
-- @~\/.ghcup\/bin/haskell-language-server-wrapper-<\hlsver\>@,
-- as well as @~\/.ghcup\/hls\/<\hlsver\>@
getInstalledHLSs :: (MonadReader env m, HasDirs env, MonadIO m, MonadCatch m)
                 => m [Either FilePath Version]
getInstalledHLSs = filter (either (const True) (safeVersion . mkTVer)) <$> do
  Dirs {..}  <- getDirs
  bins                          <- liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts compExtended
                   execBlank
                   ([s|^haskell-language-server-wrapper-.*$|] :: ByteString)
    )
  legacy <- forM bins $ \f ->
    case
          version . T.pack <$> (stripSuffix exeExt =<< stripPrefix "haskell-language-server-wrapper-" f)
      of
        Just (Right r) -> pure $ Right r
        Just (Left  _) -> pure $ Left f
        Nothing        -> pure $ Left f

  hlsdir <- ghcupHLSBaseDir
  fs     <- liftIO $ hideErrorDef [NoSuchThing] [] $ listDirectoryDirs (fromGHCupPath hlsdir)
  new <- forM fs $ \f -> case parseGHCupHLSDir f of
    Right r -> pure $ Right r
    Left  _ -> pure $ Left f
  pure (nub (new <> legacy))


-- | Get all installed stacks, by matching on
-- @~\/.ghcup\/bin/stack-<\stackver\>@.
getInstalledStacks :: (MonadReader env m, HasDirs env, MonadIO m, MonadCatch m)
                   => m [Either FilePath Version]
getInstalledStacks = filter (either (const True) (safeVersion . mkTVer)) <$> do
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
stackSet :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m, HasLog env) => m (Maybe Version)
stackSet = do
  Dirs {..}  <- getDirs
  let stackBin = binDir </> "stack" <> exeExt

  handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- liftIO $ isBrokenSymlink stackBin
    if broken
      then do
        logWarn $ "Broken symlink at " <> T.pack stackBin
        pure Nothing
      else do
        link <- liftIO
          $ handleIO' InvalidArgument
            (\e -> pure $ Left (toException e))
          $ fmap Right $ getLinkTarget stackBin
        case linkVersion =<< link of
          Right v -> pure $ Just v
          Left err -> do
            logWarn $ "Failed to parse stack symlink target with: "
              <> T.pack (displayException err)
              <> ". The symlink "
              <> T.pack stackBin
              <> " needs to point to valid stack binary, such as 'stack-2.7.1'."
            pure Nothing
 where
  linkVersion :: MonadThrow m => FilePath -> m Version
  linkVersion = throwEither . MP.parse parser "" . T.pack . dropSuffix exeExt
   where
    parser
      =   MP.try (stripAbsolutePath *> cabalParse)
      <|> MP.try (stripRelativePath *> cabalParse)
      <|> cabalParse
      <|> (_tvVersion <$> toolVersionFromPath stack)
    -- parses the version of "stack-2.7.1" -> "2.7.1"
    cabalParse = MP.chunk "stack-" *> version'
    -- parses any path component ending with path separator,
    -- e.g. "foo/"
    stripPathComponent = parseUntil1 pathSep *> MP.some pathSep
    -- parses an absolute path up until the last path separator,
    -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
    stripAbsolutePath = MP.some pathSep *> MP.many (MP.try stripPathComponent)
    -- parses a relative path up until the last path separator,
    -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
    stripRelativePath = MP.many (MP.try stripPathComponent)

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

isLegacyHLS :: (MonadIO m, MonadReader env m, HasDirs env, MonadCatch m) => Version -> m Bool
isLegacyHLS ver = do
  bdir <- ghcupHLSDir ver
  not <$> liftIO (doesDirectoryExist $ fromGHCupPath bdir)


-- Return the currently set hls version, if any.
hlsSet :: (HasLog env, MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m) => m (Maybe Version)
hlsSet = do
  Dirs {..}  <- getDirs
  let hlsBin = binDir </> "haskell-language-server-wrapper" <> exeExt

  handleIO' NoSuchThing (\_ -> pure Nothing) $ do
    broken <- liftIO $ isBrokenSymlink hlsBin
    if broken
      then do
        logWarn $ "Broken symlink at " <> T.pack hlsBin
        pure Nothing
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
      <|> (_tvVersion <$> toolVersionFromPath hls)
    -- parses the version of "haskell-language-server-wrapper-1.1.0" -> "1.1.0"
    cabalParse = MP.chunk "haskell-language-server-wrapper-" *> version'
    -- parses any path component ending with path separator,
    -- e.g. "foo/"
    stripPathComponent = parseUntil1 pathSep *> MP.some pathSep
    -- parses an absolute path up until the last path separator,
    -- e.g. "/bar/baz/foo" -> "/bar/baz/", leaving "foo"
    stripAbsolutePath = MP.some pathSep *> MP.many (MP.try stripPathComponent)
    -- parses a relative path up until the last path separator,
    -- e.g. "bar/baz/foo" -> "bar/baz/", leaving "foo"
    stripRelativePath = MP.many (MP.try stripPathComponent)


-- | Return the GHC versions the currently selected HLS supports.
hlsGHCVersions :: ( MonadReader env m
                  , HasDirs env
                  , HasLog env
                  , MonadIO m
                  , MonadThrow m
                  , MonadCatch m
                  )
               => m [Version]
hlsGHCVersions = do
  h <- hlsSet
  fromMaybe [] <$> forM h hlsGHCVersions'


hlsGHCVersions' :: ( MonadReader env m
                   , HasDirs env
                   , MonadIO m
                   , MonadThrow m
                   , MonadCatch m
                   )
                => Version
                -> m [Version]
hlsGHCVersions' v' = do
  bins <- hlsServerBinaries v' Nothing
  let vers = fmap
        (version
          . T.pack
          . fromJust
          . stripPrefix "haskell-language-server-"
          . head
          . splitOn "~"
          )
        bins
  pure . sortBy (flip compare) . rights $ vers


-- | Get all server binaries for an hls version from the ~/.ghcup/bin directory, if any.
hlsServerBinaries :: (MonadReader env m, HasDirs env, MonadIO m)
                  => Version
                  -> Maybe Version   -- ^ optional GHC version
                  -> m [FilePath]
hlsServerBinaries ver mghcVer = do
  Dirs {..}  <- getDirs
  liftIO $ handleIO (\_ -> pure []) $ findFiles
    binDir
    (makeRegexOpts
      compExtended
      execBlank
      ([s|^haskell-language-server-|]
        <> maybe [s|.*|] escapeVerRex mghcVer
        <> [s|~|]
        <> escapeVerRex ver
        <> E.encodeUtf8 (T.pack exeExt)
        <> [s|$|] :: ByteString
      )
    )

-- | Get all scripts for a hls version from the ~/.ghcup/hls/<ver>/bin directory, if any.
-- Returns the full path.
hlsInternalServerScripts :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m)
                          => Version
                          -> Maybe Version   -- ^ optional GHC version
                          -> m [FilePath]
hlsInternalServerScripts ver mghcVer = do
  dir <- ghcupHLSDir ver
  let bdir = fromGHCupPath dir </> "bin"
  fmap (bdir </>) . filter (\f -> maybe True (\gv -> ("-" <> T.unpack (prettyVer gv)) `isSuffixOf` f) mghcVer)
    <$> liftIO (listDirectoryFiles bdir)

-- | Get all binaries for a hls version from the ~/.ghcup/hls/<ver>/lib/haskell-language-server-<ver>/bin directory, if any.
-- Returns the full path.
hlsInternalServerBinaries :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadFail m)
                          => Version
                          -> Maybe Version   -- ^ optional GHC version
                          -> m [FilePath]
hlsInternalServerBinaries ver mghcVer = do
  dir <- fromGHCupPath <$> ghcupHLSDir ver
  let regex = makeRegexOpts compExtended execBlank ([s|^haskell-language-server-.*$|] :: ByteString)
  (Just bdir) <- fmap headMay $ liftIO $ expandFilePath [Left (dir </> "lib"), Right regex, Left "bin"]
  fmap (bdir </>) . filter (\f -> maybe True (\gv -> ("-" <> T.unpack (prettyVer gv)) `isSuffixOf` f) mghcVer)
    <$> liftIO (listDirectoryFiles bdir)

-- | Get all libraries for a hls version from the ~/.ghcup/hls/<ver>/lib/haskell-language-server-<ver>/lib/<ghc-ver>/
-- directory, if any.
-- Returns the full path.
hlsInternalServerLibs :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadFail m)
                      => Version
                      -> Version   -- ^ GHC version
                      -> m [FilePath]
hlsInternalServerLibs ver ghcVer = do
  dir <- fromGHCupPath <$> ghcupHLSDir ver
  let regex = makeRegexOpts compExtended execBlank ([s|^haskell-language-server-.*$|] :: ByteString)
  (Just bdir) <- fmap headMay $ liftIO $ expandFilePath [Left (dir </> "lib"), Right regex, Left ("lib" </> T.unpack (prettyVer ghcVer))]
  fmap (bdir </>) <$> liftIO (listDirectoryFiles bdir)


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
  hls'     <- hlsServerBinaries ver Nothing
  wrapper <- hlsWrapperBinary ver
  pure (maybeToList wrapper ++ hls')





    -------------
    --[ Other ]--
    -------------


-- | Usually @~\/.ghcup\/ghc\/\<ver\>\/bin\/@
ghcInternalBinDir :: (MonadReader env m, HasDirs env, MonadThrow m, MonadFail m, MonadIO m)
                  => TargetVersion
                  -> m FilePath
ghcInternalBinDir ver = do
  ghcdir <- fromGHCupPath <$> ghcupGHCDir ver
  pure (ghcdir </> "bin")


-- | Get tool files from @~\/.ghcup\/ghc\/\<ver\>\/bin\/\*@
-- while ignoring @*-\<ver\>@ symlinks and accounting for cross triple prefix.
--
-- Returns unversioned relative files without extension, e.g.:
--
--   - @["hsc2hs","haddock","hpc","runhaskell","ghc","ghc-pkg","ghci","runghc","hp2ps"]@
ghcToolFiles :: (MonadReader env m, HasDirs env, HasLog env, MonadThrow m, MonadFail m, MonadIO m)
             => TargetVersion
             -> Excepts '[NotInstalled] m [FilePath]
ghcToolFiles ver = do
  bindir <- ghcInternalBinDir ver
  logDebug2 $ "(ghcToolFiles) bindir: " <> T.pack bindir

  -- fail if ghc is not installed
  whenM (fmap not $ ghcInstalled ver)
        (throwE (NotInstalled ghc ver))

  files <- liftIO (listDirectoryFiles bindir >>= filterM (doesFileExist . (bindir </>)))
  logDebug2 $ "(ghcToolFiles) files: " <> T.pack (show files)
  let unique = getUniqueTools . groupToolFiles . fmap (dropSuffix exeExt) $ files
  logDebug2 $ "(ghcToolFiles) unique: " <> T.pack (show unique)
  pure unique

 where

  groupToolFiles :: [FilePath] -> [[(FilePath, String)]]
  groupToolFiles = groupBy (\(a, _) (b, _) -> a == b) . fmap (splitOnPVP "-")

  getUniqueTools :: [[(FilePath, String)]] -> [String]
  getUniqueTools = filter (isNotAnyInfix blackListedTools) . nub . fmap fst . concatMap (filter ((== "") . snd))

  blackListedTools :: [String]
  blackListedTools = ["haddock-ghc"]

  isNotAnyInfix :: [String] -> String -> Bool
  isNotAnyInfix xs t = foldr (\a b -> not (a `isInfixOf` t) && b) True xs




-- | For ghc without arch triple, this is:
--
--    - ghc
--
-- For ghc with arch triple:
--
--    - <triple>-ghc (e.g. arm-linux-gnueabihf-ghc)
ghcBinaryName :: TargetVersion -> String
ghcBinaryName (TargetVersion (Just t) _) = T.unpack (t <> "-ghc" <> T.pack exeExt)
ghcBinaryName (TargetVersion Nothing  _) = T.unpack ("ghc" <> T.pack exeExt)



-- | Warn if the installed and set HLS is not compatible with the installed and
-- set GHC version.
warnAboutHlsCompatibility :: ( MonadReader env m
                             , HasDirs env
                             , HasLog env
                             , MonadIOish m
                             )
                          => m ()
warnAboutHlsCompatibility = do
  supportedGHC <- hlsGHCVersions
  currentGHC   <- fmap _tvVersion <$> ghcSet Nothing
  currentHLS   <- hlsSet
  Warnings.warnAboutHlsCompatibility currentHLS currentGHC supportedGHC
