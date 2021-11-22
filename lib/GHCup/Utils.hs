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
#if defined(IS_WINDOWS)
  , module GHCup.Utils.Windows
#else
  , module GHCup.Utils.Posix
#endif
  )
where


#if defined(IS_WINDOWS)
import GHCup.Utils.Windows
#else
import GHCup.Utils.Posix
#endif
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Utils.File
import           GHCup.Utils.Logger
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Codec.Archive           hiding ( Directory )
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO( withRunInIO ) )
import           Data.Bifunctor                 ( first )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty( (:|) ))
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Versions         hiding   ( patch )
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Safe
import           System.Directory      hiding   ( findFiles )
import           System.FilePath
import           System.IO.Error
import           Text.Regex.Posix
import           URI.ByteString

import qualified Codec.Compression.BZip        as BZip
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Lzma        as Lzma
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP
import qualified Data.List.NonEmpty            as NE


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XQuasiQuotes
-- >>> import System.Directory
-- >>> import URI.ByteString
-- >>> import qualified Data.Text as T
-- >>> import GHCup.Utils.Prelude
-- >>> import GHCup.Download
-- >>> import GHCup.Version
-- >>> import GHCup.Errors
-- >>> import GHCup.Types
-- >>> import GHCup.Types.Optics
-- >>> import Optics
-- >>> import GHCup.Utils.Version.QQ
-- >>> import qualified Data.Text.Encoding as E
-- >>> import Control.Monad.Reader
-- >>> import Haskus.Utils.Variant.Excepts
-- >>> import Text.PrettyPrint.HughesPJClass ( prettyShow )
-- >>> let lc = LoggerConfig { lcPrintDebug = False, consoleOutter = mempty, fileOutter = mempty, fancyColors = False }
-- >>> dirs' <- getAllDirs
-- >>> let installedVersions = [ ([pver|8.10.7|], "-debug+lol", Nothing), ([pver|8.10.4|], "", Nothing), ([pver|8.8.4|], "", Nothing), ([pver|8.8.3|], "", Nothing) ]
-- >>> let settings = Settings True 0 False Never Curl False GHCupURL True GPGNone False
-- >>> let leanAppState = LeanAppState settings dirs' defaultKeyBindings lc
-- >>> cwd <- getCurrentDirectory
-- >>> (Right ref) <- pure $ parseURI strictURIParserOptions $ "file://" <> E.encodeUtf8 (T.pack cwd) <> "/data/metadata/" <> (urlBaseName . view pathL' $ ghcupURL)
-- >>> (VRight r) <- (fmap . fmap) _ghcupDownloads $ flip runReaderT leanAppState . runE @'[DigestError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError] $ liftE $ getBase ref



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
                   , HasLog env
                   , MonadThrow m
                   , MonadFail m
                   , MonadMask m
                   )
                => GHCTargetVersion
                -> Excepts '[NotInstalled] m ()
rmMinorSymlinks tv@GHCTargetVersion{..} = do
  Dirs {..}  <- lift getDirs

  files                         <- liftE $ ghcToolFiles tv
  forM_ files $ \f -> do
    let f_xyz = f <> "-" <> T.unpack (prettyVer _tvVersion) <> exeExt
    let fullF = binDir </> f_xyz
    lift $ logDebug ("rm -f " <> T.pack fullF)
    lift $ hideError doesNotExistErrorType $ rmLink fullF


-- | Removes the set ghc version for the given target, if any.
rmPlain :: ( MonadReader env m
           , HasDirs env
           , HasLog env
           , MonadThrow m
           , MonadFail m
           , MonadIO m
           , MonadMask m
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
      lift $ logDebug ("rm -f " <> T.pack fullF)
      lift $ hideError doesNotExistErrorType $ rmLink fullF
    -- old ghcup
    let hdc_file = binDir </> "haddock-ghc" <> exeExt
    lift $ logDebug ("rm -f " <> T.pack hdc_file)
    lift $ hideError doesNotExistErrorType $ rmLink hdc_file


-- | Remove the major GHC symlink, e.g. ghc-8.6.
rmMajorSymlinks :: ( MonadReader env m
                   , HasDirs env
                   , MonadIO m
                   , HasLog env
                   , MonadThrow m
                   , MonadFail m
                   , MonadMask m
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
    lift $ logDebug ("rm -f " <> T.pack fullF)
    lift $ hideError doesNotExistErrorType $ rmLink fullF




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
getInstalledCabals :: ( MonadReader env m
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
      then pure Nothing
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
stackSet :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m, MonadCatch m, HasLog env) => m (Maybe Version)
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


-- | Get all server binaries for an hls version, if any.
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
  hls     <- hlsServerBinaries ver Nothing
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

-- | Match PVP prefix.
--
-- >>> matchPVPrefix [pver|8.8|] [pver|8.8.4|]
-- True
-- >>> matchPVPrefix [pver|8|] [pver|8.8.4|]
-- True
-- >>> matchPVPrefix [pver|8.10|] [pver|8.8.4|]
-- False
-- >>> matchPVPrefix [pver|8.10|] [pver|8.10.7|]
-- True
matchPVPrefix :: PVP -> PVP -> Bool
matchPVPrefix (toL -> prefix) (toL -> full) = and $ zipWith (==) prefix full

toL :: PVP -> [Int]
toL (PVP inner) = fmap fromIntegral $ NE.toList inner


-- | Get the latest installed full GHC version that satisfies the given (possibly partial)
-- PVP version.
getGHCForPVP :: (MonadReader env m, HasDirs env, MonadIO m, MonadThrow m)
             => PVP
             -> Maybe Text -- ^ the target triple
             -> m (Maybe GHCTargetVersion)
getGHCForPVP pvpIn mt = do
  ghcs <- rights <$> getInstalledGHCs
  -- we're permissive here... failed parse just means we have no match anyway
  let ghcs' = catMaybes $ flip fmap ghcs $ \GHCTargetVersion{..} -> do
        (pvp_, rest) <- versionToPVP _tvVersion
        pure (pvp_, rest, _tvTarget)

  getGHCForPVP' pvpIn ghcs' mt

-- | Like 'getGHCForPVP', except with explicit input parameter.
--
-- >>> getGHCForPVP' [pver|8|] installedVersions Nothing
-- Just (GHCTargetVersion {_tvTarget = Nothing, _tvVersion = Version {_vEpoch = Nothing, _vChunks = (Digits 8 :| []) :| [Digits 10 :| [],Digits 7 :| []], _vRel = [Str "debug" :| []], _vMeta = Just "lol"}})
-- >>> fmap prettyShow $ getGHCForPVP' [pver|8.8|] installedVersions Nothing
-- "Just 8.8.4"
-- >>> fmap prettyShow $ getGHCForPVP' [pver|8.10.4|] installedVersions Nothing
-- "Just 8.10.4"
getGHCForPVP' :: MonadThrow m
             => PVP
             -> [(PVP, Text, Maybe Text)] -- ^ installed GHCs
             -> Maybe Text          -- ^ the target triple
             -> m (Maybe GHCTargetVersion)
getGHCForPVP' pvpIn ghcs' mt = do
  let mResult = lastMay
                  . sortBy (\(x, _, _) (y, _, _) -> compare x y)
                  . filter
                      (\(pvp_, _, target) ->
                        target == mt && matchPVPrefix pvp_ pvpIn
                      )
                  $ ghcs'
  forM mResult $ \(pvp_, rest, target) -> do
    ver' <- pvpToVersion pvp_ rest
    pure (GHCTargetVersion target ver')


-- | Get the latest available ghc for the given PVP version, which
-- may only contain parts.
--
-- >>> (fmap . fmap) fst $ getLatestToolFor GHC [pver|8|] r
-- Just (PVP {_pComponents = 8 :| [10,7]})
-- >>> (fmap . fmap) fst $ getLatestToolFor GHC [pver|8.8|] r
-- Just (PVP {_pComponents = 8 :| [8,4]})
-- >>> (fmap . fmap) fst $ getLatestToolFor GHC [pver|8.8.4|] r
-- Just (PVP {_pComponents = 8 :| [8,4]})
getLatestToolFor :: MonadThrow m
                 => Tool
                 -> PVP
                 -> GHCupDownloads
                 -> m (Maybe (PVP, VersionInfo))
getLatestToolFor tool pvpIn dls = do
  let ls = fromMaybe [] $ preview (ix tool % to Map.toDescList) dls
  let ps = catMaybes $ fmap (\(v, vi) -> (,vi) <$> versionToPVP v) ls
  pure . fmap (first fst) . headMay . filter (\((v, _), _) -> matchPVPrefix pvpIn v) $ ps





    -----------------
    --[ Unpacking ]--
    -----------------



-- | Unpack an archive to a temporary directory and return that path.
unpackToDir :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m)
            => FilePath       -- ^ destination dir
            -> FilePath       -- ^ archive path
            -> Excepts '[UnknownArchive
                        , ArchiveResult
                        ] m ()
unpackToDir dfp av = do
  let fn = takeFileName av
  lift $ logInfo $ "Unpacking: " <> T.pack fn <> " to " <> T.pack dfp

  let untar :: MonadIO m => BL.ByteString -> Excepts '[ArchiveResult] m ()
      untar = lEM . liftIO . runArchiveM . unpackToDirLazy dfp

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile

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
    | ".zip" `isSuffixOf` fn -> liftE (untar =<< rf av)
    | otherwise -> throwE $ UnknownArchive fn


getArchiveFiles :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m)
                => FilePath       -- ^ archive path
                -> Excepts '[UnknownArchive
                            , ArchiveResult
                            ] m [FilePath]
getArchiveFiles av = do
  let fn = takeFileName av

  let entries :: Monad m => BL.ByteString -> Excepts '[ArchiveResult] m [FilePath]
      entries = (fmap . fmap) filepath . lE . readArchiveBSL

      rf :: MonadIO m => FilePath -> Excepts '[ArchiveResult] m BL.ByteString
      rf = liftIO . BL.readFile

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
    | ".zip" `isSuffixOf` fn -> liftE (entries =<< rf av)
    | otherwise -> throwE $ UnknownArchive fn


intoSubdir :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m, MonadCatch m)
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
          -> Fold (Map.Map Version VersionInfo) (Version, VersionInfo)
getTagged tag =
  to (Map.toDescList . Map.filter (\VersionInfo {..} -> tag `elem` _viTags))
  % folding id

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

  files <- liftIO (listDirectory bindir >>= filterM (doesFileExist . (bindir </>)))
  pure (getUniqueTools . groupToolFiles . fmap (dropSuffix exeExt) $ files)

 where

  groupToolFiles :: [FilePath] -> [[(FilePath, String)]]
  groupToolFiles = groupBy (\(a, _) (b, _) -> a == b) . fmap (splitOnPVP "-")

  getUniqueTools :: [[(FilePath, String)]] -> [String]
  getUniqueTools = filter (isNotAnyInfix blackListedTools) . nub . fmap fst . filter ((== "") . snd) . concat

  blackListedTools :: [String]
  blackListedTools = ["haddock-ghc"]

  isNotAnyInfix :: [String] -> String -> Bool
  isNotAnyInfix xs t = foldr (\a b -> not (a `isInfixOf` t) && b) True xs


-- | This file, when residing in @~\/.ghcup\/ghc\/\<ver\>\/@ signals that
-- this GHC was built from source. It contains the build config.
ghcUpSrcBuiltFile :: FilePath
ghcUpSrcBuiltFile = ".ghcup_src_built"


-- | Calls gmake if it exists in PATH, otherwise make.
make :: ( MonadThrow m
        , MonadIO m
        , MonadReader env m
        , HasDirs env
        , HasLog env
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
applyPatches :: (MonadReader env m, HasDirs env, HasLog env, MonadIO m)
             => FilePath   -- ^ dir containing patches
             -> FilePath   -- ^ dir to apply patches in
             -> Excepts '[PatchFailed] m ()
applyPatches pdir ddir = do
  patches <- (fmap . fmap) (pdir </>) $ liftIO $ findFiles
      pdir
      (makeRegexOpts compExtended
                     execBlank
                     ([s|.+\.(patch|diff)$|] :: ByteString)
      )
  forM_ (sort patches) $ \patch' -> applyPatch patch' ddir


applyPatch :: (MonadReader env m, HasDirs env, HasLog env, MonadIO m)
           => FilePath   -- ^ Patch
           -> FilePath   -- ^ dir to apply patches in
           -> Excepts '[PatchFailed] m ()
applyPatch patch ddir = do
  lift $ logInfo $ "Applying patch " <> T.pack patch
  fmap (either (const Nothing) Just)
       (exec
         "patch"
         ["-p1", "-s", "-f", "-i", patch]
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
  preview (ix tool % pre (getTagged tag) % to snd % viChangeLog % _Just) dls


-- | Execute a build action while potentially cleaning up:
--
--   1. the build directory, depending on the KeepDirs setting
--   2. the install destination, depending on whether the build failed
runBuildAction :: ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadIO m
                  , MonadMask m
                  , HasLog env
                  , MonadUnliftIO m
                  , MonadFail m
                  , MonadCatch m
                  )
               => FilePath        -- ^ build directory (cleaned up depending on Settings)
               -> Maybe FilePath  -- ^ dir to *always* clean up on exception
               -> Excepts e m a
               -> Excepts e m a
runBuildAction bdir instdir action = do
  Settings {..} <- lift getSettings
  let exAction = do
        forM_ instdir $ \dir ->
          hideError doesNotExistErrorType $ recyclePathForcibly dir
        when (keepDirs == Never)
          $ rmBDir bdir
  v <-
    flip onException (lift exAction)
    $ onE_ exAction action
  when (keepDirs == Never || keepDirs == Errors) $ lift $ rmBDir bdir
  pure v


-- | Clean up the given directory if the action fails,
-- depending on the Settings.
cleanUpOnError :: ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadIO m
                  , MonadMask m
                  , HasLog env
                  , MonadUnliftIO m
                  , MonadFail m
                  , MonadCatch m
                  )
               => FilePath        -- ^ build directory (cleaned up depending on Settings)
               -> Excepts e m a
               -> Excepts e m a
cleanUpOnError bdir action = do
  Settings {..} <- lift getSettings
  let exAction = when (keepDirs == Never) $ rmBDir bdir
  flip onException (lift exAction) $ onE_ exAction action



-- | Remove a build directory, ignoring if it doesn't exist and gracefully
-- printing other errors without crashing.
rmBDir :: (MonadReader env m, HasLog env, MonadUnliftIO m, MonadIO m) => FilePath -> m ()
rmBDir dir = withRunInIO (\run -> run $
           liftIO $ handleIO (\e -> run $ logWarn $
               "Couldn't remove build dir " <> T.pack dir <> ", error was: " <> T.pack (displayException e))
           $ hideError doesNotExistErrorType
           $ rmPathForcibly dir)


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
exeExt
  | isWindows = ".exe"
  | otherwise = ""

-- | The file extension for executables.
exeExt' :: ByteString
exeExt'
  | isWindows = ".exe"
  | otherwise = ""




-- | On unix, we can use symlinks, so we just get the
-- symbolic link target.
--
-- On windows, we have to emulate symlinks via shims,
-- see 'createLink'.
getLinkTarget :: FilePath -> IO FilePath
getLinkTarget fp
  | isWindows = do
      content <- readFile (dropExtension fp <.> "shim")
      [p] <- pure . filter ("path = " `isPrefixOf`) . lines $ content
      pure $ stripNewline $ dropPrefix "path = " p
  | otherwise = getSymbolicLinkTarget fp


-- | Checks whether the path is a link.
pathIsLink :: FilePath -> IO Bool
pathIsLink fp
  | isWindows = doesPathExist (dropExtension fp <.> "shim")
  | otherwise = pathIsSymbolicLink fp


rmLink :: (MonadReader env m, HasDirs env, MonadIO m, MonadMask m) => FilePath -> m ()
rmLink fp
  | isWindows = do
      hideError doesNotExistErrorType . recycleFile $ fp
      hideError doesNotExistErrorType . recycleFile $ (dropExtension fp <.> "shim")
  | otherwise = hideError doesNotExistErrorType . recycleFile $ fp


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
              , HasLog env
              , MonadIO m
              , MonadReader env m
              , HasDirs env
              , MonadUnliftIO m
              , MonadFail m
              )
           => FilePath      -- ^ path to the target executable
           -> FilePath      -- ^ path to be created
           -> m ()
createLink link exe
  | isWindows = do
      dirs <- getDirs
      let shimGen = cacheDir dirs </> "gs.exe"

      let shim = dropExtension exe <.> "shim"
          -- For hardlinks, link needs to be absolute.
          -- If link is relative, it's relative to the target exe.
          -- Note that (</>) drops lhs when rhs is absolute.
          fullLink = takeDirectory exe </> link
          shimContents = "path = " <> fullLink

      logDebug $ "rm -f " <> T.pack exe
      rmLink exe

      logDebug $ "ln -s " <> T.pack fullLink <> " " <> T.pack exe
      liftIO $ copyFile shimGen exe
      liftIO $ writeFile shim shimContents
  | otherwise = do
      logDebug $ "rm -f " <> T.pack exe
      hideError doesNotExistErrorType $ recycleFile exe

      logDebug $ "ln -s " <> T.pack link <> " " <> T.pack exe
      liftIO $ createFileLink link exe


ensureGlobalTools :: ( MonadMask m
                     , MonadThrow m
                     , HasLog env
                     , MonadIO m
                     , MonadReader env m
                     , HasDirs env
                     , HasSettings env
                     , HasGHCupInfo env
                     , MonadUnliftIO m
                     , MonadFail m
                     )
                  => Excepts '[GPGError, DigestError , DownloadFailed, NoDownload] m ()
ensureGlobalTools
  | isWindows = do
      (GHCupInfo _ _ gTools) <- lift getGHCupInfo
      dirs <- lift getDirs
      shimDownload <- liftE $ lE @_ @'[NoDownload]
        $ maybe (Left NoDownload) Right $ Map.lookup ShimGen gTools
      let dl = downloadCached' shimDownload (Just "gs.exe") Nothing
      void $ (\DigestError{} -> do
          lift $ logWarn "Digest doesn't match, redownloading gs.exe..."
          lift $ logDebug ("rm -f " <> T.pack (cacheDir dirs </> "gs.exe"))
          lift $ hideError doesNotExistErrorType $ recycleFile (cacheDir dirs </> "gs.exe")
          liftE @'[GPGError, DigestError , DownloadFailed] $ dl
        ) `catchE` liftE @'[GPGError, DigestError , DownloadFailed] dl
  | otherwise = pure ()


-- | Ensure ghcup directory structure exists.
ensureDirectories :: Dirs -> IO ()
ensureDirectories (Dirs baseDir binDir cacheDir logsDir confDir trashDir) = do
  createDirRecursive' baseDir
  createDirRecursive' (baseDir </> "ghc")
  createDirRecursive' binDir
  createDirRecursive' cacheDir
  createDirRecursive' logsDir
  createDirRecursive' confDir
  createDirRecursive' trashDir
  pure ()


-- | For ghc without arch triple, this is:
--
--    - ghc
--
-- For ghc with arch triple:
--
--    - <triple>-ghc (e.g. arm-linux-gnueabihf-ghc)
ghcBinaryName :: GHCTargetVersion -> String
ghcBinaryName (GHCTargetVersion (Just t) _) = T.unpack (t <> "-ghc" <> T.pack exeExt)
ghcBinaryName (GHCTargetVersion Nothing  _) = T.unpack ("ghc" <> T.pack exeExt)
