{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}


module GHCup.Download where


import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
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
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder
import           Data.CaseInsensitive           ( CI )
import           Data.IORef
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text.Read
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Versions
import           GHC.IO.Exception
import           HPath
import           HPath.IO
import           Haskus.Utils.Variant.Excepts
import           Network.Http.Client     hiding ( URL )
import           OpenSSL.Digest
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.IO.Error
import "unix"    System.Posix.IO.ByteString
                                         hiding ( fdWrite )
import "unix-bytestring" System.Posix.IO.ByteString
                                                ( fdWrite )
import           System.Posix.RawFilePath.Directory.Errors
                                                ( hideError )
import           System.ProgressBar
import           URI.ByteString
import           URI.ByteString.QQ

import qualified Data.Binary.Builder           as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import qualified Data.CaseInsensitive          as CI
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified System.IO.Streams             as Streams
import qualified System.Posix.Files.ByteString as PF
import qualified System.Posix.RawFilePath.Directory
                                               as RD



ghcupURL :: URI
ghcupURL = [uri|https://www.haskell.org/ghcup/data/ghcup-0.0.1.json|]



    ------------------
    --[ High-level ]--
    ------------------


-- | Downloads the download information! But only if we need to ;P
getDownloads :: ( FromJSONKey Tool
                , FromJSONKey Version
                , FromJSON VersionInfo
                , MonadIO m
                , MonadCatch m
                , MonadReader Settings m
                , MonadLogger m
                , MonadThrow m
                , MonadFail m
                )
             => Excepts '[JSONError , DownloadFailed] m GHCupDownloads
getDownloads = do
  urlSource <- lift getUrlSource
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
           ]
          m1
          L.ByteString
  smartDl uri' = do
    let path = view pathL' uri'
    json_file <- (liftIO $ ghcupCacheDir)
      >>= \cacheDir -> (cacheDir </>) <$> urlBaseName path
    e <- liftIO $ doesFileExist json_file
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
                lift $ $(logWarn) [i|Unable to get/parse Last-Modified header|]
                liftIO $ deleteFile json_file
                liftE $ downloadBS uri'
          else -- access in less than 5 minutes, re-use file
               liftIO $ readFile json_file
      else do
        getModTime >>= \case
          Just modTime -> do
            bs <- liftE $ downloadBS uri'
            liftIO $ writeFileWithModTime modTime json_file bs
            pure bs
          Nothing -> do
            lift $ $(logWarn) [i|Unable to get/parse Last-Modified header|]
            liftE $ downloadBS uri'

   where
    getModTime = do
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
    (M.lookup (CI.mk [s|Last-Modified|]) headers) >>= \h -> parseTimeM
      True
      defaultTimeLocale
      "%a, %d %b %Y %H:%M:%S %Z"
      (T.unpack . E.decodeUtf8 $ h)

  writeFileWithModTime :: UTCTime -> Path Abs -> L.ByteString -> IO ()
  writeFileWithModTime utctime path content = do
    let mod_time = utcTimeToPOSIXSeconds utctime
    writeFileL path (Just newFilePerms) content
    setModificationTimeHiRes path mod_time



getDownloadInfo :: ( MonadLogger m
                   , MonadCatch m
                   , MonadIO m
                   , MonadReader Settings m
                   )
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
  | scheme == [s|https|] = dl
  | scheme == [s|http|] = dl
  | scheme == [s|file|] = cp
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

    (https, host, fullPath, port) <- reThrowAll DownloadFailed
      $ uriToQuadruple (view dlUri dli)

    -- destination dir must exist
    liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms dest
    destFile <- getDestFile

    -- download
    fd       <- liftIO $ createRegularFileFd newFilePerms destFile
    let stepper = fdWrite fd
    flip finally (liftIO $ closeFd fd)
      $ reThrowAll DownloadFailed
      $ downloadInternal True https host fullPath port stepper

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
                 ]
                m
                L.ByteString
downloadBS uri'
  | scheme == [s|https|]
  = dl True
  | scheme == [s|http|]
  = dl False
  | scheme == [s|file|]
  = liftIOException doesNotExistErrorType (FileDoesNotExistError path)
    $ (liftIO $ RD.readFile path)
  | otherwise
  = throwE UnsupportedScheme

 where
  scheme = view (uriSchemeL' % schemeBSL') uri'
  path   = view pathL' uri'
  dl https = do
    (_, host', fullPath', port') <- liftE $ uriToQuadruple uri'
    liftE $ downloadBS' https host' fullPath' port'


-- | Load the result of this download into memory at once.
downloadBS' :: MonadIO m
            => Bool             -- ^ https?
            -> ByteString       -- ^ host (e.g. "www.example.com")
            -> ByteString       -- ^ path (e.g. "/my/file") including query
            -> Maybe Int        -- ^ optional port (e.g. 3000)
            -> Excepts
                 '[ HTTPStatusError
                  , URIParseError
                  , UnsupportedScheme
                  , NoLocationHeader
                  , TooManyRedirs
                  ]
                 m
                 (L.ByteString)
downloadBS' https host path port = do
  bref <- liftIO $ newIORef (mempty :: Builder)
  let stepper bs = modifyIORef bref (<> byteString bs)
  downloadInternal False https host path port stepper
  liftIO (readIORef bref <&> toLazyByteString)


downloadInternal :: MonadIO m
                 => Bool        -- ^ whether to show a progress bar
                 -> Bool        -- ^ https?
                 -> ByteString  -- ^ host
                 -> ByteString  -- ^ path with query
                 -> Maybe Int   -- ^ optional port
                 -> (ByteString -> IO a)   -- ^ the consuming step function
                 -> Excepts
                      '[ HTTPStatusError
                       , URIParseError
                       , UnsupportedScheme
                       , NoLocationHeader
                       , TooManyRedirs
                       ]
                      m
                      ()
downloadInternal = go (5 :: Int)

 where
  go redirs progressBar https host path port consumer = do
    r <- liftIO $ withConnection' https host port action
    veitherToExcepts r >>= \case
      Just r' ->
        if redirs > 0 then followRedirectURL r' else throwE TooManyRedirs
      Nothing -> pure ()
   where
    action c = do
      let q = buildRequest1 $ http GET path

      sendRequest c q emptyBody

      receiveResponse
        c
        (\r i' -> runE $ do
          let scode = getStatusCode r
          if
            | scode >= 200 && scode < 300 -> downloadStream r i' >> pure Nothing
            | scode >= 300 && scode < 400 -> case getHeader r [s|Location|] of
              Just r' -> pure $ Just $ r'
              Nothing -> throwE NoLocationHeader
            | otherwise -> throwE $ HTTPStatusError scode
        )

    followRedirectURL bs = case parseURI strictURIParserOptions bs of
      Right uri' -> do
        (https', host', fullPath', port') <- liftE $ uriToQuadruple uri'
        go (redirs - 1) progressBar https' host' fullPath' port' consumer
      Left e -> throwE e

    downloadStream r i' = do
      let size = case getHeader r [s|Content-Length|] of
            Just x' -> case decimal $ E.decodeUtf8 x' of
              Left  _       -> 0
              Right (r', _) -> r'
            Nothing -> 0

      mpb <- if progressBar
        then Just <$> (liftIO $ newProgressBar defStyle 10 (Progress 0 size ()))
        else pure Nothing

      outStream <- liftIO $ Streams.makeOutputStream
        (\case
          Just bs -> do
            forM_ mpb $ \pb -> incProgress pb (BS.length bs)
            void $ consumer bs
          Nothing -> pure ()
        )
      liftIO $ Streams.connect i' outStream



getHead :: (MonadCatch m, MonadIO m)
        => URI
        -> Excepts
             '[ HTTPStatusError
              , URIParseError
              , UnsupportedScheme
              , NoLocationHeader
              , TooManyRedirs
              ]
             m
             (M.Map (CI ByteString) ByteString)
getHead uri' | scheme == [s|https|] = head' True
             | scheme == [s|http|] = head' False
             | otherwise           = throwE UnsupportedScheme

 where
  scheme = view (uriSchemeL' % schemeBSL') uri'
  head' https = do
    (_, host', fullPath', port') <- liftE $ uriToQuadruple uri'
    liftE $ headInternal https host' fullPath' port'



headInternal :: MonadIO m
             => Bool        -- ^ https?
             -> ByteString  -- ^ host
             -> ByteString  -- ^ path with query
             -> Maybe Int   -- ^ optional port
             -> Excepts
                  '[ HTTPStatusError
                   , URIParseError
                   , UnsupportedScheme
                   , TooManyRedirs
                   , NoLocationHeader
                   ]
                  m
                  (M.Map (CI ByteString) ByteString)
headInternal = go (5 :: Int)

 where
  go redirs https host path port = do
    r <- liftIO $ withConnection' https host port action
    veitherToExcepts r >>= \case
      Left r' ->
        if redirs > 0 then followRedirectURL r' else throwE TooManyRedirs
      Right hs -> pure hs
   where

    action c = do
      let q = buildRequest1 $ http HEAD path

      sendRequest c q emptyBody

      unsafeReceiveResponse
        c
        (\r _ -> runE $ do
          let scode = getStatusCode r
          if
            | scode >= 200 && scode < 300 -> do
              let headers = getHeaderMap r
              pure $ Right $ headers
            | scode >= 300 && scode < 400 -> case getHeader r [s|Location|] of
              Just r' -> pure $ Left $ r'
              Nothing -> throwE NoLocationHeader
            | otherwise -> throwE $ HTTPStatusError scode
        )

    followRedirectURL bs = case parseURI strictURIParserOptions bs of
      Right uri' -> do
        (https', host', fullPath', port') <- liftE $ uriToQuadruple uri'
        go (redirs - 1) https' host' fullPath' port'
      Left e -> throwE e


withConnection' :: Bool
                -> ByteString
                -> Maybe Int
                -> (Connection -> IO a)
                -> IO a
withConnection' https host port action = bracket acquire closeConnection action

 where
  acquire = case https of
    True -> do
      ctx <- baselineContextSSL
      openConnectionSSL ctx host (fromIntegral $ fromMaybe 443 port)
    False -> openConnection host (fromIntegral $ fromMaybe 80 port)


-- | Extracts from a URI type: (https?, host, path+query, port)
uriToQuadruple :: Monad m
               => URI
               -> Excepts
                    '[UnsupportedScheme]
                    m
                    (Bool, ByteString, ByteString, Maybe Int)
uriToQuadruple URI {..} = do
  let scheme = view schemeBSL' uriScheme

  host <-
    preview (_Just % authorityHostL' % hostBSL') uriAuthority
      ?? UnsupportedScheme

  https <- if
    | scheme == [s|https|] -> pure True
    | scheme == [s|http|] -> pure False
    | otherwise           -> throwE UnsupportedScheme

  let
    queryBS =
      BS.intercalate [s|&|]
        . fmap (\(x, y) -> encodeQuery x <> [s|=|] <> encodeQuery y)
        $ (queryPairs uriQuery)
    port =
      preview (_Just % authorityPortL' % _Just % portNumberL') uriAuthority
    fullpath =
      if BS.null queryBS then uriPath else uriPath <> [s|?|] <> queryBS
  pure (https, host, fullpath, port)
  where encodeQuery = L.toStrict . B.toLazyByteString . urlEncodeQuery


checkDigest :: (MonadIO m, MonadLogger m, MonadReader Settings m)
            => DownloadInfo
            -> Path Abs
            -> Excepts '[DigestError] m ()
checkDigest dli file = do
  verify <- lift ask <&> (not . noVerify)
  when verify $ do
    let p' = toFilePath file
    lift $ $(logInfo) [i|veryfing digest of: #{p'}|]
    c <- liftIO $ readFile file
    let cDigest = E.decodeUtf8 . toHex . digest (digestByName "sha256") $ c
        eDigest = view dlHash dli
    when ((cDigest /= eDigest) && verify) $ throwE (DigestError cDigest eDigest)
