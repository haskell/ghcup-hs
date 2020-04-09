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

#if !defined(CURL)
import           GHCup.Download.IOStreams
import           GHCup.Download.Utils
#endif
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
#if defined(CURL)
import           GHCup.Utils.File
#endif
import           GHCup.Utils.Prelude
import           GHCup.Version

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Data.CaseInsensitive           ( CI )
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Versions
import           GHC.IO.Exception
import           HPath
import           HPath.IO                      as HIO
import           Haskus.Utils.Variant.Excepts
import           OpenSSL.Digest
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.IO.Error
import           URI.ByteString

import qualified Data.ByteString.Lazy          as L
import qualified Data.CaseInsensitive          as CI
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified System.Posix.Files.ByteString as PF
import qualified System.Posix.RawFilePath.Directory
                                               as RD






    ------------------
    --[ High-level ]--
    ------------------


-- | Downloads the download information! But only if we need to ;P
getDownloads :: ( FromJSONKey Tool
                , FromJSONKey Version
                , FromJSON VersionInfo
                , MonadIO m
                , MonadCatch m
                , MonadLogger m
                , MonadThrow m
                , MonadFail m
                )
             => URLSource
             -> Excepts '[JSONError , DownloadFailed] m GHCupDownloads
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
           . (MonadCatch m1, MonadIO m1, MonadFail m1, MonadLogger m1)
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
                  then do
                    bs <- liftE $ downloadBS uri'
                    liftIO $ writeFileWithModTime modTime json_file bs
                    pure bs
                  else liftIO $ readFile json_file
              Nothing -> do
                lift $ $(logDebug) [i|Unable to get/parse Last-Modified header|]
                liftIO $ deleteFile json_file
                liftE $ downloadBS uri'
          else -- access in less than 5 minutes, re-use file
               liftIO $ readFile json_file
      else do
        liftIO $ createDirIfMissing newDirPerms cacheDir
        getModTime >>= \case
          Just modTime -> do
            bs <- liftE $ downloadBS uri'
            liftIO $ writeFileWithModTime modTime json_file bs
            pure bs
          Nothing -> do
            lift $ $(logDebug) [i|Unable to get/parse Last-Modified header|]
            liftE $ downloadBS uri'

   where
    getModTime = do
#if defined(CURL)
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
#endif

  parseModifiedHeader :: (M.Map (CI ByteString) ByteString) -> Maybe UTCTime
  parseModifiedHeader headers =
    (M.lookup (CI.mk "Last-Modified") headers) >>= \h -> parseTimeM
      True
      defaultTimeLocale
      "%a, %d %b %Y %H:%M:%S %Z"
      (T.unpack . E.decodeUtf8 $ h)

  writeFileWithModTime :: UTCTime -> Path Abs -> L.ByteString -> IO ()
  writeFileWithModTime utctime path content = do
    let mod_time = utcTimeToPOSIXSeconds utctime
    writeFileL path (Just newFilePerms) content
    setModificationTimeHiRes path mod_time



getDownloadInfo :: (MonadLogger m, MonadCatch m, MonadIO m)
                => GHCupDownloads
                -> Tool
                -> Version
                -> Maybe PlatformRequest
                -> Excepts
                     '[ DistroNotFound
                      , NoCompatiblePlatform
                      , NoCompatibleArch
                      , NoDownload
                      ]
                     m
                     DownloadInfo
getDownloadInfo bDls t v mpfReq = do
  (PlatformRequest arch' plat ver) <- case mpfReq of
    Just x  -> pure x
    Nothing -> do
      (PlatformResult rp rv) <- liftE getPlatform
      ar                     <- lE getArchitecture
      pure $ PlatformRequest ar rp rv

  lE $ getDownloadInfo' t v arch' plat ver bDls


getDownloadInfo' :: Tool
                 -> Version
                -- ^ tool version
                 -> Architecture
                -- ^ user arch
                 -> Platform
                -- ^ user platform
                 -> Maybe Versioning
                -- ^ optional version of the platform
                 -> GHCupDownloads
                 -> Either NoDownload DownloadInfo
getDownloadInfo' t v a p mv dls = maybe
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
    let uri' = E.decodeUtf8 (serializeURIRef' (view dlUri dli))
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
#if defined(CURL)
              liftE $ lEM @_ @'[ProcessError] $ liftIO $ exec "curl" True
                ["-sSfL", "-o", toFilePath destFile , serializeURIRef' $ view dlUri dli] Nothing Nothing
#else
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
downloadBS :: (MonadCatch m, MonadIO m)
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
  dl https = do
#if defined(CURL)
    let exe = [rel|curl|]
        args = ["-sSfL", serializeURIRef' uri']
    liftIO (executeOut exe args Nothing) >>= \case
      CapturedProcess ExitSuccess stdout _ -> do
        pure $ L.fromStrict stdout
      CapturedProcess (ExitFailure i') _ _ -> throwE $ NonZeroExit i' (toFilePath exe) args
#else
    (_, host', fullPath', port') <- liftE $ uriToQuadruple uri'
    liftE $ downloadBS' https host' fullPath' port'
#endif


checkDigest :: (MonadIO m, MonadLogger m, MonadReader Settings m)
            => DownloadInfo
            -> Path Abs
            -> Excepts '[DigestError] m ()
checkDigest dli file = do
  verify <- lift ask <&> (not . noVerify)
  when verify $ do
    let p' = toFilePath file
    lift $ $(logInfo) [i|verifying digest of: #{p'}|]
    c <- liftIO $ readFile file
    let cDigest = E.decodeUtf8 . toHex . digest (digestByName "sha256") $ c
        eDigest = view dlHash dli
    when ((cDigest /= eDigest) && verify) $ throwE (DigestError cDigest eDigest)

