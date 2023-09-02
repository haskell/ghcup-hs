{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


{-|
Module      : GHCup.Download
Description : Downloading
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

Module for handling all download related functions.

Generally we support downloading via:

  - curl (default)
  - wget
  - internal downloader (only when compiled)
-}
module GHCup.Download where

#if defined(INTERNAL_DOWNLOADER)
import           GHCup.Download.IOStreams
import           GHCup.Download.Utils
#endif
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Dirs
import           GHCup.Prelude
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger.Internal
import           GHCup.Prelude.Process
import           GHCup.Version

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.Aeson
import           Data.ByteString                ( ByteString )
#if defined(INTERNAL_DOWNLOADER)
import           Data.CaseInsensitive           ( mk )
#endif
import           Data.Maybe
import           Data.List
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Versions
import           Data.Word8              hiding ( isSpace )
import           Haskus.Utils.Variant.Excepts
#if defined(INTERNAL_DOWNLOADER)
import           Network.Http.Client     hiding ( URL )
#endif
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           Safe
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Data.Yaml.Aeson               as Y






    ------------------
    --[ High-level ]--
    ------------------



-- | Downloads the download information! But only if we need to ;P
getDownloadsF :: ( FromJSONKey Tool
                 , FromJSONKey Version
                 , FromJSON VersionInfo
                 , MonadReader env m
                 , HasSettings env
                 , HasDirs env
                 , MonadIO m
                 , MonadCatch m
                 , HasLog env
                 , MonadThrow m
                 , MonadFail m
                 , MonadMask m
                 )
              => Excepts
                   '[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError]
                   m
                   GHCupInfo
getDownloadsF = do
  Settings { urlSource } <- lift getSettings
  case urlSource of
    GHCupURL -> liftE $ getBase ghcupURL
    (OwnSource exts) -> do
      ext  <- liftE $ mapM (either pure getBase) exts
      mergeGhcupInfo ext
    (OwnSpec av) -> pure av
    (AddSource exts) -> do
      base <- liftE $ getBase ghcupURL
      ext  <- liftE $ mapM (either pure getBase) exts
      mergeGhcupInfo (base:ext)

 where
  mergeGhcupInfo :: MonadFail m
                 => [GHCupInfo]
                 -> m GHCupInfo
  mergeGhcupInfo [] = fail "mergeGhcupInfo: internal error: need at least one GHCupInfo"
  mergeGhcupInfo xs@(GHCupInfo{}: _) =
    let newDownloads   = M.unionsWith (M.unionWith (\_ b2 -> b2)) (_ghcupDownloads   <$> xs)
        newGlobalTools = M.unionsWith (\_ a2 -> a2              ) (_globalTools      <$> xs)
        newToolReqs    = M.unionsWith (M.unionWith (\_ b2 -> b2)) (_toolRequirements <$> xs)
    in pure $ GHCupInfo newToolReqs newDownloads newGlobalTools


yamlFromCache :: (MonadReader env m, HasDirs env) => URI -> m FilePath
yamlFromCache uri = do
  Dirs{..} <- getDirs
  pure (fromGHCupPath cacheDir </> (T.unpack . decUTF8Safe . urlBaseName . view pathL' $ uri))


etagsFile :: FilePath -> FilePath
etagsFile = (<.> "etags")


getBase :: ( MonadReader env m
           , HasDirs env
           , HasSettings env
           , MonadFail m
           , MonadIO m
           , MonadCatch m
           , HasLog env
           , MonadMask m
           )
        => URI
        -> Excepts '[DownloadFailed, GPGError, DigestError, ContentLengthError, JSONError, FileDoesNotExistError] m GHCupInfo
getBase uri = do
  Settings { noNetwork, downloader, metaMode } <- lift getSettings

  -- try to download yaml... usually this writes it into cache dir,
  -- but in some cases not (e.g. when using file://), so we honour
  -- the return filepath, if any
  mYaml <- if noNetwork && view (uriSchemeL' % schemeBSL') uri /= "file" -- for file://, let it fall through
           then pure Nothing
           else handleIO (\e -> case metaMode of
                                  Strict -> throwIO e
                                  Lax -> lift (warnCache (displayException e) downloader) >> pure Nothing)
               . catchE @_ @_ @'[DownloadFailed] (\e@(DownloadFailed _) -> case metaMode of
                   Strict -> throwE e
                   Lax -> lift (warnCache (prettyHFError e) downloader) >> pure Nothing)
               . fmap Just
               . smartDl
               $ uri

  -- if we didn't get a filepath from the download, use the cached yaml
  actualYaml <- maybe (lift $ yamlFromCache uri) pure mYaml
  lift $ logDebug $ "Decoding yaml at: " <> T.pack actualYaml

  liftE
    . onE_ (onError actualYaml)
    . lEM' @_ @_ @'[JSONError] (\(displayException -> e) -> JSONDecodeError $ unlines [e, "Consider removing " <> actualYaml <> " manually."])
    . liftIO
    . Y.decodeFileEither
    $ actualYaml
 where
  -- On error, remove the etags file and set access time to 0. This should ensure the next invocation
  -- may re-download and succeed.
  onError :: (MonadReader env m, HasLog env, MonadMask m, MonadCatch m, MonadIO m) => FilePath -> m ()
  onError fp = do
    let efp = etagsFile fp
    handleIO (\e -> logWarn $ "Couldn't remove file " <> T.pack efp <> ", error was: " <> T.pack (displayException e))
      (hideError doesNotExistErrorType $ rmFile efp)
    liftIO $ hideError doesNotExistErrorType $ setAccessTime fp (posixSecondsToUTCTime (fromIntegral @Int 0))

  warnCache :: (MonadReader env m, HasLog env, MonadMask m, MonadCatch m, MonadIO m) => FilePath -> Downloader -> m ()
  warnCache s downloader' = do
    let tryDownloder = case downloader' of
                         Curl -> "Wget"
                         Wget -> "Curl"
#if defined(INTERNAL_DOWNLOADER)
                         Internal -> "Curl"
#endif
    logWarn $ "Could not get download info, trying cached version (this may not be recent!)" <> "\n" <>
      "If this problem persists, consider switching downloader via: " <> "\n    " <>
      "ghcup config set downloader " <> tryDownloder
    logDebug $ "Error was: " <> T.pack s

  -- First check if the json file is in the ~/.ghcup/cache dir
  -- and check it's access time. If it has been accessed within the
  -- last 5 minutes, just reuse it.
  --
  -- Always save the local file with the mod time of the remote file.
  smartDl :: forall m1 env1
           . ( MonadReader env1 m1
             , HasDirs env1
             , HasSettings env1
             , MonadCatch m1
             , MonadIO m1
             , MonadFail m1
             , HasLog env1
             , MonadMask m1
             )
          => URI
          -> Excepts
               '[ DownloadFailed
                , DigestError
                , ContentLengthError
                , GPGError
                ]
               m1
               FilePath
  smartDl uri' = do
    json_file <- lift $ yamlFromCache uri'
    let scheme = view (uriSchemeL' % schemeBSL') uri'
    e <- liftIO $ doesFileExist json_file
    currentTime <- liftIO getCurrentTime
    Dirs { cacheDir } <- lift getDirs
    Settings { metaCache } <- lift getSettings

       -- for local files, let's short-circuit and ignore access time
    if | scheme == "file" -> liftE $ download uri' (Just $ over pathL' (<> ".sig") uri') Nothing Nothing (fromGHCupPath cacheDir) Nothing True
       | e -> do
          accessTime <- fmap utcTimeToPOSIXSeconds $ liftIO $ getAccessTime json_file
          let sinceLastAccess = utcTimeToPOSIXSeconds currentTime - accessTime
          let cacheInterval = fromInteger metaCache
          lift $ logDebug $ "last access was " <> T.pack (show sinceLastAccess) <> " ago, cache interval is " <> T.pack (show cacheInterval)
          -- access time won't work on most linuxes, but we can try regardless
          if | metaCache <= 0 -> dlWithMod currentTime json_file
             | (sinceLastAccess > cacheInterval) ->
                -- no access in last 5 minutes, re-check upstream mod time
                dlWithMod currentTime json_file
             | otherwise -> pure json_file
       | otherwise -> dlWithMod currentTime json_file
   where
    dlWithMod modTime json_file = do
      let (dir, fn) = splitFileName json_file
      f <- liftE $ download uri' (Just $ over pathL' (<> ".sig") uri') Nothing Nothing dir (Just fn) True

      -- make these failures non-fatal, also see:
      -- https://github.com/actions/runner-images/issues/7061
      handleIO (\e -> logWarn $ "setModificationTime failed with: " <> T.pack (displayException e)) $ liftIO $ setModificationTime f modTime
      handleIO (\e -> logWarn $ "setAccessTime failed with: " <> T.pack (displayException e)) $ liftIO $ setAccessTime f modTime

      pure f

getDownloadInfo :: ( MonadReader env m
                   , HasPlatformReq env
                   , HasGHCupInfo env
                   )
                => Tool
                -> Version
                -- ^ tool version
                -> Excepts
                     '[NoDownload]
                     m
                     DownloadInfo
getDownloadInfo t v = getDownloadInfo' t (mkTVer v)

getDownloadInfo' :: ( MonadReader env m
                    , HasPlatformReq env
                    , HasGHCupInfo env
                    )
                 => Tool
                 -> GHCTargetVersion
                 -- ^ tool version
                 -> Excepts
                      '[NoDownload]
                      m
                      DownloadInfo
getDownloadInfo' t v = do
  pfreq@(PlatformRequest a p mv) <- lift getPlatformReq
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  let distro_preview f g =
        let platformVersionSpec =
              preview (ix t % ix v % viArch % ix a % ix (f p)) dls
            mv' = g mv
        in  fmap snd
              .   find
                    (\(mverRange, _) -> maybe
                      (isNothing mv')
                      (\range -> maybe False (`versionRange` range) mv')
                      mverRange
                    )
              .   M.toList
              =<< platformVersionSpec
      with_distro        = distro_preview id id
      without_distro_ver = distro_preview id (const Nothing)
      without_distro     = distro_preview (set _Linux UnknownLinux) (const Nothing)

  maybe
    (throwE $ NoDownload v t (Just pfreq))
    pure
    (case p of
      -- non-musl won't work on alpine
      Linux Alpine -> with_distro <|> without_distro_ver
      _            -> with_distro <|> without_distro_ver <|> without_distro
    )


-- | Tries to download from the given http or https url
-- and saves the result in continuous memory into a file.
-- If the filename is not provided, then we:
--   1. try to guess the filename from the url path
--   2. otherwise create a random file
--
-- The file must not exist.
download :: ( MonadReader env m
            , HasSettings env
            , HasDirs env
            , MonadMask m
            , MonadThrow m
            , HasLog env
            , MonadIO m
            )
         => URI
         -> Maybe URI         -- ^ URI for gpg sig
         -> Maybe T.Text      -- ^ expected hash
         -> Maybe Integer     -- ^ expected content length
         -> FilePath          -- ^ destination dir (ignored for file:// scheme)
         -> Maybe FilePath    -- ^ optional filename
         -> Bool              -- ^ whether to read an write etags
         -> Excepts '[DigestError, ContentLengthError, DownloadFailed, GPGError] m FilePath
download rawUri gpgUri eDigest eCSize dest mfn etags
  | scheme == "https" = liftE dl
  | scheme == "http"  = liftE dl
  | scheme == "file"
  , Just s <- gpgScheme
  , s /= "file" = throwIO $ userError $ "gpg scheme does not match base file scheme: " <> (T.unpack . decUTF8Safe $ s)
  | scheme == "file"  = do
      Settings{ gpgSetting } <- lift getSettings
      let destFile' = T.unpack . decUTF8Safe $ view pathL' rawUri
      lift $ logDebug $ "using local file: " <> T.pack destFile'
      liftE $ verify gpgSetting destFile' (pure . T.unpack . decUTF8Safe . view pathL')
      pure destFile'
  | otherwise = throwE $ DownloadFailed (variantFromValue UnsupportedScheme)

 where
  scheme    = view (uriSchemeL' % schemeBSL') rawUri
  gpgScheme = view (uriSchemeL' % schemeBSL') <$> gpgUri
  dl = do
    Settings{ mirrors } <- lift getSettings
    let uri = applyMirrors mirrors rawUri
    baseDestFile <- liftE . reThrowAll @_ @_ @'[DownloadFailed] DownloadFailed $ getDestFile uri mfn
    lift $ logInfo $ "downloading: " <> (decUTF8Safe . serializeURIRef') uri <> " as file " <> T.pack baseDestFile

    -- destination dir must exist
    liftIO $ createDirRecursive' dest


    -- download
    flip onException
         (lift $ hideError doesNotExistErrorType $ recycleFile (tmpFile baseDestFile))
     $ catchAllE @_ @'[GPGError, ProcessError, DownloadFailed, UnsupportedScheme, DigestError, ContentLengthError] @'[DigestError, ContentLengthError, DownloadFailed, GPGError]
          (\e' -> do
            lift $ hideError doesNotExistErrorType $ recycleFile (tmpFile baseDestFile)
            case e' of
              V e@GPGError {} -> throwE e
              V e@DigestError {} -> throwE e
              _ -> throwE (DownloadFailed e')
          ) $ do
              Settings{ downloader, noNetwork, gpgSetting } <- lift getSettings
              when noNetwork $ throwE (DownloadFailed (V NoNetwork :: V '[NoNetwork]))
              downloadAction <- case downloader of
                    Curl -> do
                      o' <- liftIO getCurlOpts
                      if etags
                        then pure $ curlEtagsDL o'
                        else pure $ curlDL o'
                    Wget -> do
                      o' <- liftIO getWgetOpts
                      if etags
                        then pure $ wgetEtagsDL o'
                        else pure $ wgetDL o'
#if defined(INTERNAL_DOWNLOADER)
                    Internal -> do
                      if etags
                        then pure (\fp -> liftE . internalEtagsDL fp)
                        else pure (\fp -> liftE . internalDL fp)
#endif
              liftE $ downloadAction baseDestFile uri
              liftE $ verify gpgSetting baseDestFile
                (\uri' -> do
                  gpgDestFile <- liftE . reThrowAll @_ @_ @'[DownloadFailed] DownloadFailed $ getDestFile uri' Nothing
                  lift $ logDebug $ "downloading: " <> (decUTF8Safe . serializeURIRef') uri' <> " as file " <> T.pack gpgDestFile
                  flip onException (lift $ hideError doesNotExistErrorType $ recycleFile (tmpFile gpgDestFile)) $
                    downloadAction gpgDestFile uri'
                  pure gpgDestFile
                )
    pure baseDestFile

  curlDL :: ( MonadCatch m
            , MonadMask m
            , MonadIO m
            )
         => [String]
         -> FilePath
         -> URI
         -> Excepts '[ProcessError, DownloadFailed, UnsupportedScheme] m ()
  curlDL o' destFile (decUTF8Safe . serializeURIRef' -> uri') = do
    let destFileTemp = tmpFile destFile
    flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
      liftE $ lEM @_ @'[ProcessError] $ exec "curl"
        (o' ++ ["-fL", "-o", destFileTemp, T.unpack uri']
            ++ maybe [] (\s -> ["--max-filesize", show s]) eCSize
        ) Nothing Nothing
      liftIO $ renameFile destFileTemp destFile

  curlEtagsDL :: ( MonadReader env m
                 , HasLog env
                 , MonadCatch m
                 , MonadMask m
                 , MonadIO m
                 )
              => [String]
              -> FilePath
              -> URI
              -> Excepts '[ProcessError, DownloadFailed, UnsupportedScheme] m ()
  curlEtagsDL o' destFile (decUTF8Safe . serializeURIRef' -> uri') = do
    let destFileTemp = tmpFile destFile
    dh <- liftIO $ emptySystemTempFile "curl-header"
    flip finally (try @_ @SomeException $ rmFile dh) $
      flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
        metag <- lift $ readETag destFile
        liftE $ lEM @_ @'[ProcessError] $ exec "curl"
            (o' ++ (if etags then ["--dump-header", dh] else [])
                ++ maybe [] (\t -> ["-H", "If-None-Match: " <> T.unpack t]) metag
                ++ ["-fL", "-o", destFileTemp, T.unpack uri']) Nothing Nothing
        headers <- liftIO $ T.readFile dh

        -- this nonsense is necessary, because some older versions of curl would overwrite
        -- the destination file when 304 is returned
        case fmap T.words . listToMaybe . fmap T.strip . T.lines . getLastHeader $ headers of
          Just (http':sc:_)
            | sc == "304"
            , T.pack "HTTP" `T.isPrefixOf` http' -> lift $ logDebug "Status code was 304, not overwriting"
            | T.pack "HTTP" `T.isPrefixOf` http' -> do
                lift $ logDebug $ "Status code was " <> sc <> ", overwriting"
                liftIO $ renameFile destFileTemp destFile
          _ -> liftE $ throwE @_ @'[DownloadFailed] (DownloadFailed (toVariantAt @0 (MalformedHeaders headers)
            :: V '[MalformedHeaders]))

        lift $ writeEtags destFile (parseEtags headers)

  wgetDL :: ( MonadCatch m
            , MonadMask m
            , MonadIO m
            )
         => [String]
         -> FilePath
         -> URI
         -> Excepts '[ProcessError, DownloadFailed, UnsupportedScheme] m ()
  wgetDL o' destFile (decUTF8Safe . serializeURIRef' -> uri') = do
    let destFileTemp = tmpFile destFile
    flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
      let opts = o' ++ ["-O", destFileTemp , T.unpack uri']
      liftE $ lEM @_ @'[ProcessError] $ exec "wget" opts Nothing Nothing
      liftIO $ renameFile destFileTemp destFile


  wgetEtagsDL :: ( MonadReader env m
                 , HasLog env
                 , MonadCatch m
                 , MonadMask m
                 , MonadIO m
                 )
              => [String]
              -> FilePath
              -> URI
              -> Excepts '[ProcessError, DownloadFailed, UnsupportedScheme] m ()
  wgetEtagsDL o' destFile (decUTF8Safe . serializeURIRef' -> uri') = do
    let destFileTemp = tmpFile destFile
    flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
      metag <- lift $ readETag destFile
      let opts = o' ++ maybe [] (\t -> ["--header", "If-None-Match: " <> T.unpack t]) metag
                    ++ ["-q", "-S", "-O", destFileTemp , T.unpack uri']
      CapturedProcess {_exitCode, _stdErr} <- lift $ executeOut "wget" opts Nothing
      case _exitCode of
        ExitSuccess -> do
          liftIO $ renameFile destFileTemp destFile
          lift $ writeEtags destFile (parseEtags (decUTF8Safe' _stdErr))
        ExitFailure i'
          | i' == 8
          , Just _ <- find (T.pack "304 Not Modified" `T.isInfixOf`) . T.lines . decUTF8Safe' $ _stdErr
                   -> do
                        lift $ logDebug "Not modified, skipping download"
                        lift $ writeEtags destFile (parseEtags (decUTF8Safe' _stdErr))
          | otherwise -> throwE (NonZeroExit i' "wget" opts)

#if defined(INTERNAL_DOWNLOADER)
  internalDL :: ( MonadCatch m
                , MonadMask m
                , MonadIO m
                )
             => FilePath -> URI -> Excepts '[DownloadFailed, UnsupportedScheme] m ()
  internalDL destFile uri' = do
    let destFileTemp = tmpFile destFile
    flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
      (https, host, fullPath, port) <- liftE $ uriToQuadruple uri'
      void $ liftE $ catchE @HTTPNotModified
                 @'[DownloadFailed]
            (\e@(HTTPNotModified _) ->
              throwE @_ @'[DownloadFailed] (DownloadFailed (toVariantAt @0 e :: V '[HTTPNotModified])))
        $ downloadToFile https host fullPath port destFileTemp mempty eCSize
      liftIO $ renameFile destFileTemp destFile


  internalEtagsDL :: ( MonadReader env m
                     , HasLog env
                     , MonadCatch m
                     , MonadMask m
                     , MonadIO m
                     )
                  => FilePath -> URI -> Excepts '[DownloadFailed, UnsupportedScheme] m ()
  internalEtagsDL destFile uri' = do
    let destFileTemp = tmpFile destFile
    flip finally (try @_ @SomeException $ rmFile destFileTemp) $ do
      (https, host, fullPath, port) <- liftE $ uriToQuadruple uri'
      metag <- lift $ readETag destFile
      let addHeaders = maybe mempty (\etag -> M.fromList [ (mk . E.encodeUtf8 . T.pack $ "If-None-Match"
                                                         , E.encodeUtf8 etag)]) metag
      liftE
        $ catchE @HTTPNotModified @'[DownloadFailed] @'[] (\(HTTPNotModified etag) -> lift $ writeEtags destFile (pure $ Just etag))
        $ do
          r <- downloadToFile https host fullPath port destFileTemp addHeaders eCSize
          liftIO $ renameFile destFileTemp destFile
          lift $ writeEtags destFile (pure $ decUTF8Safe <$> getHeader r "etag")
#endif


  -- Manage to find a file we can write the body into.
  getDestFile :: Monad m => URI -> Maybe FilePath -> Excepts '[NoUrlBase] m FilePath
  getDestFile uri' mfn' =
    let path = view pathL' uri'
    in case mfn' of
        Just fn -> pure (dest </> fn)
        Nothing
          | let urlBase = T.unpack (decUTF8Safe (urlBaseName path))
          , not (null urlBase) -> pure (dest </> urlBase)
          -- TODO: remove this once we use hpath again
          | otherwise -> throwE $ NoUrlBase (decUTF8Safe . serializeURIRef' $ uri')

  parseEtags :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m) => T.Text -> m (Maybe T.Text)
  parseEtags stderr = do
    let mEtag = find (\line -> T.pack "etag:" `T.isPrefixOf` T.toLower line) . fmap T.strip . T.lines . getLastHeader $ stderr
    case T.words <$> mEtag of
      (Just []) -> do
        logDebug "Couldn't parse etags, no input: "
        pure Nothing
      (Just [_, etag']) -> do
        logDebug $ "Parsed etag: " <> etag'
        pure (Just etag')
      (Just xs) -> do
        logDebug ("Couldn't parse etags, unexpected input: " <> T.unwords xs)
        pure Nothing
      Nothing -> do
        logDebug "No etags header found"
        pure Nothing

  writeEtags :: (MonadReader env m, HasLog env, MonadIO m, MonadThrow m) => FilePath -> m (Maybe T.Text) -> m ()
  writeEtags destFile getTags = do
    getTags >>= \case
      Just t -> do
        logDebug $ "Writing etagsFile " <> T.pack (etagsFile destFile)
        liftIO $ T.writeFile (etagsFile destFile) t
      Nothing ->
        logDebug "No etags files written"

  readETag :: (MonadReader env m, HasLog env, MonadCatch m, MonadIO m) => FilePath -> m (Maybe T.Text)
  readETag fp = do
    e <- liftIO $ doesFileExist fp
    if e
    then do
      rE <- try @_ @SomeException $ liftIO $ fmap stripNewline' $ T.readFile (etagsFile fp)
      case rE of
        (Right et) -> do
          logDebug $ "Read etag: " <> et
          pure (Just et)
        (Left _) -> do
          logDebug "Etag file doesn't exist (yet)"
          pure Nothing
    else do
      logDebug $ "Skipping and deleting etags file because destination file " <> T.pack fp <> " doesn't exist"
      liftIO $ hideError doesNotExistErrorType $ rmFile (etagsFile fp)
      pure Nothing

  verify :: ( MonadReader env m
            , HasLog env
            , HasDirs env
            , HasSettings env
            , MonadCatch m
            , MonadMask m
            , MonadIO m
            )
            => GPGSetting
            -> FilePath
            -> (URI -> Excepts '[ProcessError, DownloadFailed, UnsupportedScheme] m FilePath)
            -> Excepts '[DigestError, ContentLengthError, DownloadFailed, GPGError] m ()
  verify gpgSetting destFile' downloadAction' = do
    case (gpgUri, gpgSetting) of
      (_, GPGNone) -> pure ()
      (Just gpgUri', _) -> do
        liftE $ catchAllE @_ @'[GPGError, ProcessError, UnsupportedScheme, DownloadFailed] @'[GPGError]
              (\e -> if gpgSetting == GPGStrict then throwE (GPGError e) else lift $ logWarn $ T.pack (prettyHFError (GPGError e))
              ) $ do
            o' <- liftIO getGpgOpts
            gpgDestFile <- liftE $ downloadAction' gpgUri'
            lift $ logInfo $ "verifying signature of: " <> T.pack destFile'
            let args = o' ++ ["--batch", "--verify", "--quiet", "--no-tty", gpgDestFile, destFile']
            cp <- lift $ executeOut "gpg" args Nothing
            case cp of
              CapturedProcess { _exitCode = ExitFailure i, _stdErr } -> do
                lift $ logDebug $ decUTF8Safe' _stdErr
                throwE (GPGError @'[ProcessError] (V (NonZeroExit i "gpg" args)))
              CapturedProcess { _stdErr } -> lift $ logDebug $ decUTF8Safe' _stdErr
      _ -> pure ()

    forM_ eCSize  (liftE . flip checkCSize  destFile')
    forM_ eDigest (liftE . flip checkDigest destFile')



-- | Download into tmpdir or use cached version, if it exists. If filename
-- is omitted, infers the filename from the url.
downloadCached :: ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadMask m
                  , MonadResource m
                  , MonadThrow m
                  , HasLog env
                  , MonadIO m
                  , MonadUnliftIO m
                  )
               => DownloadInfo
               -> Maybe FilePath  -- ^ optional filename
               -> Excepts '[DigestError, ContentLengthError, DownloadFailed, GPGError] m FilePath
downloadCached dli mfn = do
  Settings{ cache } <- lift getSettings
  case cache of
    True -> liftE $ downloadCached' dli mfn Nothing
    False -> do
      tmp <- lift withGHCupTmpDir
      liftE $ download (_dlUri dli) Nothing (Just (_dlHash dli)) (_dlCSize dli) (fromGHCupPath tmp) outputFileName False
 where
  outputFileName = mfn <|> _dlOutput dli


downloadCached' :: ( MonadReader env m
                   , HasDirs env
                   , HasSettings env
                   , MonadMask m
                   , MonadThrow m
                   , HasLog env
                   , MonadIO m
                   , MonadUnliftIO m
                   )
                => DownloadInfo
                -> Maybe FilePath  -- ^ optional filename
                -> Maybe FilePath  -- ^ optional destination dir (default: cacheDir)
                -> Excepts '[DigestError, ContentLengthError, DownloadFailed, GPGError] m FilePath
downloadCached' dli mfn mDestDir = do
  Dirs { cacheDir } <- lift getDirs
  let destDir = fromMaybe (fromGHCupPath cacheDir) mDestDir
  let fn = fromMaybe ((T.unpack . decUTF8Safe) $ urlBaseName $ view (dlUri % pathL') dli) outputFileName
  let cachfile = destDir </> fn
  fileExists <- liftIO $ doesFileExist cachfile
  if
    | fileExists -> do
      forM_ (view dlCSize dli) $ \s -> liftE $ checkCSize s cachfile
      liftE $ checkDigest (view dlHash dli) cachfile
      pure cachfile
    | otherwise -> liftE $ download (_dlUri dli) Nothing (Just (_dlHash dli)) (_dlCSize dli) destDir outputFileName False
 where
  outputFileName = mfn <|> _dlOutput dli




    ------------------
    --[ Low-level ]--
    ------------------



checkDigest :: ( MonadReader env m
               , HasDirs env
               , HasSettings env
               , MonadIO m
               , MonadThrow m
               , HasLog env
               )
            => T.Text     -- ^ the hash
            -> FilePath
            -> Excepts '[DigestError] m ()
checkDigest eDigest file = do
  Settings{ noVerify } <- lift getSettings
  let verify = not noVerify
  when verify $ do
    let p' = takeFileName file
    lift $ logInfo $ "verifying digest of: " <> T.pack p'
    c <- liftIO $ L.readFile file
    cDigest <- throwEither . E.decodeUtf8' . B16.encode . SHA256.hashlazy $ c
    when ((cDigest /= eDigest) && verify) $ throwE (DigestError file cDigest eDigest)

checkCSize :: ( MonadReader env m
              , HasDirs env
              , HasSettings env
              , MonadIO m
              , MonadThrow m
              , HasLog env
              )
           => Integer
           -> FilePath
           -> Excepts '[ContentLengthError] m ()
checkCSize eCSize file = do
  Settings{ noVerify } <- lift getSettings
  let verify = not noVerify
  when verify $ do
    let p' = takeFileName file
    lift $ logInfo $ "verifying content length of: " <> T.pack p'
    cSize <- liftIO $ getFileSize file
    when ((eCSize /= cSize) && verify) $ throwE (ContentLengthError (Just file) (Just cSize) eCSize)


-- | Get additional curl args from env. This is an undocumented option.
getCurlOpts :: IO [String]
getCurlOpts =
  lookupEnv "GHCUP_CURL_OPTS" >>= \case
    Just r  -> pure $ splitOn " " r
    Nothing -> pure []


-- | Get additional wget args from env. This is an undocumented option.
getWgetOpts :: IO [String]
getWgetOpts =
  lookupEnv "GHCUP_WGET_OPTS" >>= \case
    Just r  -> pure $ splitOn " " r
    Nothing -> pure []

-- | Get additional gpg args from env. This is an undocumented option.
getGpgOpts :: IO [String]
getGpgOpts =
  lookupEnv "GHCUP_GPG_OPTS" >>= \case
    Just r  -> pure $ splitOn " " r
    Nothing -> pure []

-- | Get the url base name.
--
-- >>> urlBaseName "/foo/bar/baz"
-- "baz"
urlBaseName :: ByteString  -- ^ the url path (without scheme and host)
            -> ByteString
urlBaseName = snd . B.breakEnd (== _slash) . urlDecode False


-- | Curl saves all intermediate connect headers as well, not just the last one, so we make an effort to take the
-- last HTTP block only. Passing '--suppress-connect-headers' would be better, but it isn't supported by all versions,
-- also see:
--   https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/213
--
-- >>> getLastHeader "\n\nHTTP/1.0 200 Connection established\n\nHTTP/1.1 304 Not Modified\n"
-- "HTTP/1.1 304 Not Modified\n"
-- >>> getLastHeader "HTTP/1.1 304 Not Modified\n"
-- "HTTP/1.1 304 Not Modified\n"
getLastHeader :: T.Text -> T.Text
getLastHeader = T.unlines . lastDef [] . filter (\x -> not (null x)) . splitOn [""] . fmap T.stripEnd . T.lines


tmpFile :: FilePath -> FilePath
tmpFile = (<.> "tmp")


applyMirrors :: DownloadMirrors -> URI -> URI
applyMirrors (DM ms) uri@(URI { uriAuthority = Just (Authority { authorityHost = Host host }) }) =
  case M.lookup (decUTF8Safe host) ms of
    Nothing -> uri
    Just (DownloadMirror auth (Just prefix)) ->
      uri { uriAuthority = Just auth
          , uriPath = E.encodeUtf8 $ T.pack ("/" <> T.unpack prefix <> (T.unpack . decUTF8Safe . uriPath $ uri))
          }
    Just (DownloadMirror auth Nothing) ->
      uri { uriAuthority = Just auth }
applyMirrors _ uri = uri

