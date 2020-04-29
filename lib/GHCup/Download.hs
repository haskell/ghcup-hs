{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}


module GHCup.Download where

#if defined(INTERNAL_DOWNLOADER)
import           GHCup.Download.IOStreams
import           GHCup.Download.Utils
#endif
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Prelude
import           GHCup.Version

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
import           Data.Aeson
import           Data.ByteString                ( ByteString )
#if defined(INTERNAL_DOWNLOADER)
import           Data.CaseInsensitive           ( CI )
#endif
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
#if defined(INTERNAL_DOWNLOADER)
import           Data.Time.Format
#endif
import           Data.Versions
import           Data.Word8
import           GHC.IO.Exception
import           HPath
import           HPath.IO                      as HIO
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.IO.Error
import           System.Posix.Env.ByteString    ( getEnv )
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as L
#if defined(INTERNAL_DOWNLOADER)
import qualified Data.CaseInsensitive          as CI
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
#endif
import qualified Data.Text.Encoding            as E
import qualified System.Posix.Files.ByteString as PF
import qualified System.Posix.RawFilePath.Directory
                                               as RD






    ------------------
    --[ High-level ]--
    ------------------


-- | Like 'getDownloads', but tries to fall back to
-- cached ~/.ghcup/cache/ghcup-<format-ver>.json
getDownloadsF :: ( FromJSONKey Tool
                 , FromJSONKey Version
                 , FromJSON VersionInfo
                 , MonadIO m
                 , MonadCatch m
                 , MonadLogger m
                 , MonadThrow m
                 , MonadFail m
                 , MonadReader Settings m
                 )
              => URLSource
              -> Excepts
                   '[JSONError , DownloadFailed , FileDoesNotExistError]
                   m
                   GHCupInfo
getDownloadsF urlSource = do
  case urlSource of
    GHCupURL ->
      liftE
        $ handleIO (\_ -> readFromCache)
        $ catchE @_ @'[JSONError , FileDoesNotExistError]
            (\(DownloadFailed _) -> readFromCache)
        $ getDownloads urlSource
    (OwnSource _) -> liftE $ getDownloads urlSource
    (OwnSpec   _) -> liftE $ getDownloads urlSource
 where
  readFromCache = do
    lift $ $(logWarn)
      [i|Could not get download info, trying cached version (this may not be recent!)|]
    let path = view pathL' ghcupURL
    cacheDir  <- liftIO $ ghcupCacheDir
    json_file <- (cacheDir </>) <$> urlBaseName path
    bs        <-
      handleIO' NoSuchThing
                (\_ -> throwE $ FileDoesNotExistError (toFilePath json_file))
      $ liftIO
      $ readFile json_file
    lE' JSONDecodeError $ eitherDecode' bs


-- | Downloads the download information! But only if we need to ;P
getDownloads :: ( FromJSONKey Tool
                , FromJSONKey Version
                , FromJSON VersionInfo
                , MonadIO m
                , MonadCatch m
                , MonadLogger m
                , MonadThrow m
                , MonadFail m
                , MonadReader Settings m
                )
             => URLSource
             -> Excepts '[JSONError , DownloadFailed] m GHCupInfo
getDownloads urlSource = do
  lift $ $(logDebug) [i|Receiving download info from: #{urlSource}|]
  case urlSource of
    GHCupURL -> do
      bs <- reThrowAll DownloadFailed $ smartDl ghcupURL
      lE' JSONDecodeError $ eitherDecode' bs
    (OwnSource url) -> do
      bs <- reThrowAll DownloadFailed $ downloadBS url
      lE' JSONDecodeError $ eitherDecode' bs
    (OwnSpec av) -> pure $ av

 where
  -- First check if the json file is in the ~/.ghcup/cache dir
  -- and check it's access time. If it has been accessed within the
  -- last 5 minutes, just reuse it.
  --
  -- If not, then send a HEAD request and check for modification time.
  -- Only download the file if the modification time is newer
  -- than the local file.
  --
  -- Always save the local file with the mod time of the remote file.
  smartDl :: forall m1
           . ( MonadCatch m1
             , MonadIO m1
             , MonadFail m1
             , MonadLogger m1
             , MonadReader Settings m1
             )
          => URI
          -> Excepts
               '[ FileDoesNotExistError
                , HTTPStatusError
                , URIParseError
                , UnsupportedScheme
                , NoLocationHeader
                , TooManyRedirs
                , ProcessError
                ]
               m1
               L.ByteString
  smartDl uri' = do
    let path = view pathL' uri'
    cacheDir  <- liftIO $ ghcupCacheDir
    json_file <- (cacheDir </>) <$> urlBaseName path
    e         <- liftIO $ doesFileExist json_file
    if e
      then do
        accessTime <-
          PF.accessTimeHiRes
            <$> (liftIO $ PF.getFileStatus (toFilePath json_file))
        currentTime <- liftIO $ getPOSIXTime

        -- access time won't work on most linuxes, but we can try regardless
        if (currentTime - accessTime) > 300
          then do -- no access in last 5 minutes, re-check upstream mod time
            getModTime >>= \case
              Just modTime -> do
                fileMod <- liftIO $ getModificationTime json_file
                if modTime > fileMod
                  then dlWithMod modTime json_file
                  else liftIO $ readFile json_file
              Nothing -> do
                lift $ $(logDebug) [i|Unable to get/parse Last-Modified header|]
                dlWithoutMod json_file
          else -- access in less than 5 minutes, re-use file
               liftIO $ readFile json_file
      else do
        liftIO $ createDirIfMissing newDirPerms cacheDir
        getModTime >>= \case
          Just modTime -> dlWithMod modTime json_file
          Nothing -> do
            -- although we don't know last-modified, we still save
            -- it to a file, so we might use it in offline mode
            lift $ $(logDebug) [i|Unable to get/parse Last-Modified header|]
            dlWithoutMod json_file

   where
    dlWithMod modTime json_file = do
      bs <- liftE $ downloadBS uri'
      liftIO $ writeFileWithModTime modTime json_file bs
      pure bs
    dlWithoutMod json_file = do
      bs <- liftE $ downloadBS uri'
      liftIO $ hideError doesNotExistErrorType $ deleteFile json_file
      liftIO $ writeFileL json_file (Just newFilePerms) bs
      liftIO $ setModificationTime json_file (fromIntegral @Int 0)
      pure bs


    getModTime = do
#if !defined(INTERNAL_DOWNLOADER)
      pure Nothing
#else
      headers <-
        handleIO (\_ -> pure mempty)
        $ liftE
        $ ( catchAllE
              (\_ ->
                pure mempty :: Excepts '[] m1 (M.Map (CI ByteString) ByteString)
              )
          $ getHead uri'
          )
      pure $ parseModifiedHeader headers

  parseModifiedHeader :: (M.Map (CI ByteString) ByteString) -> Maybe UTCTime
  parseModifiedHeader headers =
    (M.lookup (CI.mk "Last-Modified") headers) >>= \h -> parseTimeM
      True
      defaultTimeLocale
      "%a, %d %b %Y %H:%M:%S %Z"
      (T.unpack . decUTF8Safe $ h)

#endif

  writeFileWithModTime :: UTCTime -> Path Abs -> L.ByteString -> IO ()
  writeFileWithModTime utctime path content = do
    let mod_time = utcTimeToPOSIXSeconds utctime
    writeFileL path (Just newFilePerms) content
    setModificationTimeHiRes path mod_time


getDownloadInfo :: Tool
                -> Version
                -- ^ tool version
                -> PlatformRequest
                -> GHCupDownloads
                -> Either NoDownload DownloadInfo
getDownloadInfo t v (PlatformRequest a p mv) dls = maybe
  (Left NoDownload)
  Right
  (with_distro <|> without_distro_ver <|> without_distro)

 where
  with_distro        = distro_preview id id
  without_distro_ver = distro_preview id (const Nothing)
  without_distro     = distro_preview (set _Linux UnknownLinux) (const Nothing)

  distro_preview f g =
    preview (ix t % ix v % viArch % ix a % ix (f p) % ix (g mv)) dls


-- | Tries to download from the given http or https url
-- and saves the result in continuous memory into a file.
-- If the filename is not provided, then we:
--   1. try to guess the filename from the url path
--   2. otherwise create a random file
--
-- The file must not exist.
download :: ( MonadMask m
            , MonadReader Settings m
            , MonadThrow m
            , MonadLogger m
            , MonadIO m
            )
         => DownloadInfo
         -> Path Abs          -- ^ destination dir
         -> Maybe (Path Rel)  -- ^ optional filename
         -> Excepts '[DigestError , DownloadFailed] m (Path Abs)
download dli dest mfn
  | scheme == "https" = dl
  | scheme == "http"  = dl
  | scheme == "file"  = cp
  | otherwise = throwE $ DownloadFailed (variantFromValue UnsupportedScheme)

 where
  scheme = view (dlUri % uriSchemeL' % schemeBSL') dli
  cp     = do
    -- destination dir must exist
    liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms dest
    destFile <- getDestFile
    fromFile <- parseAbs path
    liftIO $ copyFile fromFile destFile Strict
    pure destFile
  dl = do
    let uri' = decUTF8Safe (serializeURIRef' (view dlUri dli))
    lift $ $(logInfo) [i|downloading: #{uri'}|]

    -- destination dir must exist
    liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms dest
    destFile <- getDestFile

    -- download
    flip onException
         (liftIO $ hideError doesNotExistErrorType $ deleteFile destFile)
     $ catchAllE @_ @'[ProcessError, DownloadFailed, UnsupportedScheme]
          (\e ->
            (liftIO $ hideError doesNotExistErrorType $ deleteFile destFile)
              >> (throwE . DownloadFailed $ e)
          ) $ do
              lift getDownloader >>= \case
                Curl -> do
                  o' <- liftIO getCurlOpts
                  liftE $ lEM @_ @'[ProcessError] $ liftIO $ exec "curl" True
                    (o' ++ ["-fL", "-o", toFilePath destFile, serializeURIRef' $ view dlUri dli]) Nothing Nothing
                Wget -> do
                  o' <- liftIO getWgetOpts
                  liftE $ lEM @_ @'[ProcessError] $ liftIO $ exec "wget" True
                    (o' ++ ["-O", toFilePath destFile , serializeURIRef' $ view dlUri dli]) Nothing Nothing
#if defined(INTERNAL_DOWNLOADER)
                Internal -> do
                  (https, host, fullPath, port) <- liftE $ uriToQuadruple (view dlUri dli)
                  liftE $ downloadToFile https host fullPath port destFile
#endif

    liftE $ checkDigest dli destFile
    pure destFile

  -- Manage to find a file we can write the body into.
  getDestFile :: MonadThrow m => m (Path Abs)
  getDestFile = maybe (urlBaseName path <&> (dest </>)) (pure . (dest </>)) mfn

  path        = view (dlUri % pathL') dli


-- | Download into tmpdir or use cached version, if it exists. If filename
-- is omitted, infers the filename from the url.
downloadCached :: ( MonadMask m
                  , MonadResource m
                  , MonadThrow m
                  , MonadLogger m
                  , MonadIO m
                  , MonadReader Settings m
                  )
               => DownloadInfo
               -> Maybe (Path Rel)  -- ^ optional filename
               -> Excepts '[DigestError , DownloadFailed] m (Path Abs)
downloadCached dli mfn = do
  cache <- lift getCache
  case cache of
    True -> do
      cachedir <- liftIO $ ghcupCacheDir
      fn       <- maybe (urlBaseName $ view (dlUri % pathL') dli) pure mfn
      let cachfile = cachedir </> fn
      fileExists <- liftIO $ doesFileExist cachfile
      if
        | fileExists -> do
          liftE $ checkDigest dli cachfile
          pure $ cachfile
        | otherwise -> liftE $ download dli cachedir mfn
    False -> do
      tmp <- lift withGHCupTmpDir
      liftE $ download dli tmp mfn




    ------------------
    --[ Low-level ]--
    ------------------




-- | This is used for downloading the JSON.
downloadBS :: (MonadReader Settings m, MonadCatch m, MonadIO m, MonadLogger m)
           => URI
           -> Excepts
                '[ FileDoesNotExistError
                 , HTTPStatusError
                 , URIParseError
                 , UnsupportedScheme
                 , NoLocationHeader
                 , TooManyRedirs
                 , ProcessError
                 ]
                m
                L.ByteString
downloadBS uri'
  | scheme == "https"
  = dl True
  | scheme == "http"
  = dl False
  | scheme == "file"
  = liftIOException doesNotExistErrorType (FileDoesNotExistError path)
    $ (liftIO $ RD.readFile path)
  | otherwise
  = throwE UnsupportedScheme

 where
  scheme = view (uriSchemeL' % schemeBSL') uri'
  path   = view pathL' uri'
#if defined(INTERNAL_DOWNLOADER)
  dl https = do
#else
  dl _ = do
#endif
    lift $ $(logDebug) [i|downloading: #{serializeURIRef' uri'}|]
    lift getDownloader >>= \case
      Curl -> do
        o' <- liftIO getCurlOpts
        let exe = [rel|curl|]
            args = o' ++ ["-sSfL", serializeURIRef' uri']
        liftIO (executeOut exe args Nothing) >>= \case
          CapturedProcess ExitSuccess stdout _ -> do
            pure $ L.fromStrict stdout
          CapturedProcess (ExitFailure i') _ _ -> throwE $ NonZeroExit i' (toFilePath exe) args
      Wget -> do
        o' <- liftIO getWgetOpts
        let exe = [rel|wget|]
            args = o' ++ ["-qO-", serializeURIRef' uri']
        liftIO (executeOut exe args Nothing) >>= \case
          CapturedProcess ExitSuccess stdout _ -> do
            pure $ L.fromStrict stdout
          CapturedProcess (ExitFailure i') _ _ -> throwE $ NonZeroExit i' (toFilePath exe) args
#if defined(INTERNAL_DOWNLOADER)
      Internal -> do
        (_, host', fullPath', port') <- liftE $ uriToQuadruple uri'
        liftE $ downloadBS' https host' fullPath' port'
#endif


checkDigest :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader Settings m)
            => DownloadInfo
            -> Path Abs
            -> Excepts '[DigestError] m ()
checkDigest dli file = do
  verify <- lift ask <&> (not . noVerify)
  when verify $ do
    p' <- toFilePath <$> basename file
    lift $ $(logInfo) [i|verifying digest of: #{p'}|]
    c <- liftIO $ readFile file
    cDigest <- throwEither . E.decodeUtf8' . B16.encode . SHA256.hashlazy $ c
    let eDigest = view dlHash dli
    when ((cDigest /= eDigest) && verify) $ throwE (DigestError cDigest eDigest)


-- | Get additional curl args from env. This is an undocumented option.
getCurlOpts :: IO [ByteString]
getCurlOpts =
  getEnv "GHCUP_CURL_OPTS" >>= \case
    Just r  -> pure $ BS.split _space r
    Nothing -> pure []


-- | Get additional wget args from env. This is an undocumented option.
getWgetOpts :: IO [ByteString]
getWgetOpts =
  getEnv "GHCUP_WGET_OPTS" >>= \case
    Just r  -> pure $ BS.split _space r
    Nothing -> pure []

