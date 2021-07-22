{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils.Dirs
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
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
#if defined(INTERNAL_DOWNLOADER)
import           Data.CaseInsensitive           ( CI )
#endif
import           Data.List.Extra
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
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as L
import qualified Data.Map.Strict               as M
#if defined(INTERNAL_DOWNLOADER)
import qualified Data.CaseInsensitive          as CI
#endif
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Yaml                     as Y






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
                 , MonadLogger m
                 , MonadThrow m
                 , MonadFail m
                 , MonadMask m
                 )
              => Excepts
                   '[JSONError , DownloadFailed , FileDoesNotExistError]
                   m
                   GHCupInfo
getDownloadsF = do
  Settings { urlSource } <- lift getSettings
  case urlSource of
    GHCupURL -> liftE $ getBase ghcupURL
    (OwnSource url) -> liftE $ getBase url
    (OwnSpec av) -> pure av
    (AddSource (Left ext)) -> do
      base <- liftE $ getBase ghcupURL
      pure (mergeGhcupInfo base ext)
    (AddSource (Right uri)) -> do
      base <- liftE $ getBase ghcupURL
      ext  <- liftE $ getBase uri
      pure (mergeGhcupInfo base ext)

    where

  mergeGhcupInfo :: GHCupInfo -- ^ base to merge with
                 -> GHCupInfo -- ^ extension overwriting the base
                 -> GHCupInfo
  mergeGhcupInfo (GHCupInfo tr base base2) (GHCupInfo _ ext ext2) =
    let newDownloads = M.mapWithKey (\k a -> case M.lookup k ext of
                                        Just a' -> M.union a' a
                                        Nothing -> a
                                    ) base
        newGlobalTools = M.union base2 ext2
    in GHCupInfo tr newDownloads newGlobalTools


readFromCache :: ( MonadReader env m
                 , HasDirs env
                 , MonadIO m
                 , MonadCatch m)
              => URI
              -> Excepts '[JSONError, FileDoesNotExistError] m L.ByteString
readFromCache uri = do
  Dirs{..} <- lift getDirs
  let yaml_file = cacheDir </> (T.unpack . decUTF8Safe . urlBaseName . view pathL' $ uri)
  handleIO' NoSuchThing (\_ -> throwE $ FileDoesNotExistError yaml_file)
    . liftIO
    . L.readFile
    $ yaml_file


getBase :: ( MonadReader env m
           , HasDirs env
           , HasSettings env
           , MonadFail m
           , MonadIO m
           , MonadCatch m
           , MonadLogger m
           , MonadMask m
           )
        => URI
        -> Excepts '[JSONError , FileDoesNotExistError] m GHCupInfo
getBase uri = do
  Settings { noNetwork } <- lift getSettings
  bs <- if noNetwork
        then readFromCache uri
        else handleIO (\_ -> warnCache >> readFromCache uri)
               . catchE @_ @'[JSONError, FileDoesNotExistError] (\(DownloadFailed _) -> warnCache >> readFromCache uri)
               . reThrowAll @_ @_ @'[JSONError, DownloadFailed] DownloadFailed
               $ smartDl uri
  liftE
    . lE' @_ @_ @'[JSONError] JSONDecodeError
    . first show
    . Y.decodeEither'
    . L.toStrict
    $ bs
 where
  warnCache = lift $ $(logWarn)
      [i|Could not get download info, trying cached version (this may not be recent!)|]

  -- First check if the json file is in the ~/.ghcup/cache dir
  -- and check it's access time. If it has been accessed within the
  -- last 5 minutes, just reuse it.
  --
  -- If not, then send a HEAD request and check for modification time.
  -- Only download the file if the modification time is newer
  -- than the local file.
  --
  -- Always save the local file with the mod time of the remote file.
  smartDl :: forall m1 env1
           . ( MonadReader env1 m1
             , HasDirs env1
             , HasSettings env1
             , MonadCatch m1
             , MonadIO m1
             , MonadFail m1
             , MonadLogger m1
             , MonadMask m1
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
                , NoNetwork
                ]
               m1
               L.ByteString
  smartDl uri' = do
    Dirs{..} <- lift getDirs
    let path = view pathL' uri'
    let json_file = cacheDir </> (T.unpack . decUTF8Safe . urlBaseName $ path)
    e <- liftIO $ doesFileExist json_file
    if e
      then do
        accessTime <- liftIO $ getAccessTime json_file
        currentTime <- liftIO getCurrentTime

        -- access time won't work on most linuxes, but we can try regardless
        if (utcTimeToPOSIXSeconds currentTime - utcTimeToPOSIXSeconds accessTime) > 300
          then do -- no access in last 5 minutes, re-check upstream mod time
            getModTime >>= \case
              Just modTime -> do
                fileMod <- liftIO $ getModificationTime json_file
                if modTime > fileMod
                  then dlWithMod modTime json_file
                  else liftIO $ L.readFile json_file
              Nothing -> do
                lift $ $(logDebug) [i|Unable to get/parse Last-Modified header|]
                dlWithoutMod json_file
          else -- access in less than 5 minutes, re-use file
               liftIO $ L.readFile json_file
      else do
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
      lift $ hideError doesNotExistErrorType $ recycleFile json_file
      liftIO $ L.writeFile json_file bs
      liftIO $ setModificationTime json_file (posixSecondsToUTCTime (fromIntegral @Int 0))
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

  writeFileWithModTime :: UTCTime -> FilePath -> L.ByteString -> IO ()
  writeFileWithModTime utctime path content = do
    L.writeFile path content
    setModificationTime path utctime


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
getDownloadInfo t v = do
  (PlatformRequest a p mv) <- lift getPlatformReq
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
    (throwE NoDownload)
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
            , MonadLogger m
            , MonadIO m
            )
         => DownloadInfo
         -> FilePath          -- ^ destination dir
         -> Maybe FilePath    -- ^ optional filename
         -> Excepts '[DigestError , DownloadFailed] m FilePath
download dli dest mfn
  | scheme == "https" = dl
  | scheme == "http"  = dl
  | scheme == "file"  = cp
  | otherwise = throwE $ DownloadFailed (variantFromValue UnsupportedScheme)

 where
  scheme = view (dlUri % uriSchemeL' % schemeBSL') dli
  cp     = do
    -- destination dir must exist
    liftIO $ createDirRecursive' dest
    let destFile = getDestFile
    let fromFile = T.unpack . decUTF8Safe $ path
    liftIO $ copyFile fromFile destFile
    pure destFile
  dl = do
    let uri' = decUTF8Safe (serializeURIRef' (view dlUri dli))
    lift $ $(logInfo) [i|downloading: #{uri'}|]

    -- destination dir must exist
    liftIO $ createDirRecursive' dest
    let destFile = getDestFile

    -- download
    flip onException
         (lift $ hideError doesNotExistErrorType $ recycleFile destFile)
     $ catchAllE @_ @'[ProcessError, DownloadFailed, UnsupportedScheme]
          (\e ->
            lift (hideError doesNotExistErrorType $ recycleFile destFile)
              >> (throwE . DownloadFailed $ e)
          ) $ do
              Settings{ downloader, noNetwork } <- lift getSettings
              when noNetwork $ throwE (DownloadFailed (V NoNetwork :: V '[NoNetwork]))
              case downloader of
                Curl -> do
                  o' <- liftIO getCurlOpts
                  liftE $ lEM @_ @'[ProcessError] $ exec "curl" 
                    (o' ++ ["-fL", "-o", destFile, (T.unpack . decUTF8Safe) $ serializeURIRef' $ view dlUri dli]) Nothing Nothing
                Wget -> do
                  o' <- liftIO getWgetOpts
                  liftE $ lEM @_ @'[ProcessError] $ exec "wget" 
                    (o' ++ ["-O", destFile , (T.unpack . decUTF8Safe) $ serializeURIRef' $ view dlUri dli]) Nothing Nothing
#if defined(INTERNAL_DOWNLOADER)
                Internal -> do
                  (https, host, fullPath, port) <- liftE $ uriToQuadruple (view dlUri dli)
                  liftE $ downloadToFile https host fullPath port destFile
#endif

    liftE $ checkDigest dli destFile
    pure destFile

  -- Manage to find a file we can write the body into.
  getDestFile :: FilePath
  getDestFile = maybe (dest </> T.unpack (decUTF8Safe (urlBaseName path)))
                  (dest </>)
                  mfn

  path = view (dlUri % pathL') dli


-- | Download into tmpdir or use cached version, if it exists. If filename
-- is omitted, infers the filename from the url.
downloadCached :: ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadMask m
                  , MonadResource m
                  , MonadThrow m
                  , MonadLogger m
                  , MonadIO m
                  , MonadUnliftIO m
                  )
               => DownloadInfo
               -> Maybe FilePath  -- ^ optional filename
               -> Excepts '[DigestError , DownloadFailed] m FilePath
downloadCached dli mfn = do
  Settings{ cache } <- lift getSettings
  case cache of
    True -> downloadCached' dli mfn Nothing
    False -> do
      tmp <- lift withGHCupTmpDir
      liftE $ download dli tmp mfn


downloadCached' :: ( MonadReader env m
                   , HasDirs env
                   , HasSettings env
                   , MonadMask m
                   , MonadThrow m
                   , MonadLogger m
                   , MonadIO m
                   , MonadUnliftIO m
                   )
                => DownloadInfo
                -> Maybe FilePath  -- ^ optional filename
                -> Maybe FilePath  -- ^ optional destination dir (default: cacheDir)
                -> Excepts '[DigestError , DownloadFailed] m FilePath
downloadCached' dli mfn mDestDir = do
  Dirs { cacheDir } <- lift getDirs
  let destDir = fromMaybe cacheDir mDestDir
  let fn = fromMaybe ((T.unpack . decUTF8Safe) $ urlBaseName $ view (dlUri % pathL') dli) mfn
  let cachfile = destDir </> fn
  fileExists <- liftIO $ doesFileExist cachfile
  if
    | fileExists -> do
      liftE $ checkDigest dli cachfile
      pure cachfile
    | otherwise -> liftE $ download dli destDir mfn




    ------------------
    --[ Low-level ]--
    ------------------




-- | This is used for downloading the JSON.
downloadBS :: ( MonadReader env m
              , HasSettings env
              , MonadCatch m
              , MonadIO m
              , MonadLogger m
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
                 , NoNetwork
                 ]
                m
                L.ByteString
downloadBS uri'
  | scheme == "https"
  = dl True
  | scheme == "http"
  = dl False
  | scheme == "file"
  = liftIOException doesNotExistErrorType (FileDoesNotExistError $ T.unpack $ decUTF8Safe path)
    (liftIO $ L.readFile (T.unpack $ decUTF8Safe path))
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
    Settings{ downloader, noNetwork } <- lift getSettings
    when noNetwork $ throwE NoNetwork
    case downloader of
      Curl -> do
        o' <- liftIO getCurlOpts
        let exe = "curl"
            args = o' ++ ["-sSfL", T.unpack $ decUTF8Safe $ serializeURIRef' uri']
        lift (executeOut exe args Nothing) >>= \case
          CapturedProcess ExitSuccess stdout _ -> do
            pure stdout
          CapturedProcess (ExitFailure i') _ _ -> throwE $ NonZeroExit i' exe args
      Wget -> do
        o' <- liftIO getWgetOpts
        let exe = "wget"
            args = o' ++ ["-qO-", T.unpack $ decUTF8Safe $ serializeURIRef' uri']
        lift (executeOut exe args Nothing) >>= \case
          CapturedProcess ExitSuccess stdout _ -> do
            pure stdout
          CapturedProcess (ExitFailure i') _ _ -> throwE $ NonZeroExit i' exe args
#if defined(INTERNAL_DOWNLOADER)
      Internal -> do
        (_, host', fullPath', port') <- liftE $ uriToQuadruple uri'
        liftE $ downloadBS' https host' fullPath' port'
#endif


checkDigest :: ( MonadReader env m
               , HasDirs env
               , HasSettings env
               , MonadIO m
               , MonadThrow m
               , MonadLogger m
               )
            => DownloadInfo
            -> FilePath
            -> Excepts '[DigestError] m ()
checkDigest dli file = do
  Settings{ noVerify } <- lift getSettings
  let verify = not noVerify
  when verify $ do
    let p' = takeFileName file
    lift $ $(logInfo) [i|verifying digest of: #{p'}|]
    c <- liftIO $ L.readFile file
    cDigest <- throwEither . E.decodeUtf8' . B16.encode . SHA256.hashlazy $ c
    let eDigest = view dlHash dli
    when ((cDigest /= eDigest) && verify) $ throwE (DigestError cDigest eDigest)


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


urlBaseName :: ByteString  -- ^ the url path (without scheme and host)
            -> ByteString
urlBaseName = snd . B.breakEnd (== _slash) . urlDecode False

