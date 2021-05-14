{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module GHCup.Download.IOStreams where


import           GHCup.Download.Utils
import           GHCup.Errors
import           GHCup.Types.Optics
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.File
import           GHCup.Utils.Prelude

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Builder
import           Data.CaseInsensitive           ( CI )
import           Data.IORef
import           Data.Maybe
import           Data.Text.Read
import           Haskus.Utils.Variant.Excepts
import           Network.Http.Client     hiding ( URL )
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.ProgressBar
import           System.IO
import           URI.ByteString

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import qualified Data.Map.Strict               as M
import qualified System.IO.Streams             as Streams





    ----------------------------
    --[ Low-level (non-curl) ]--
    ----------------------------


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
                 L.ByteString
downloadBS' https host path port = do
  bref <- liftIO $ newIORef (mempty :: Builder)
  let stepper bs = modifyIORef bref (<> byteString bs)
  downloadInternal False https host path port stepper
  liftIO (readIORef bref <&> toLazyByteString)


downloadToFile :: (MonadMask m, MonadIO m)
               => Bool             -- ^ https?
               -> ByteString       -- ^ host (e.g. "www.example.com")
               -> ByteString       -- ^ path (e.g. "/my/file") including query
               -> Maybe Int        -- ^ optional port (e.g. 3000)
               -> FilePath         -- ^ destination file to create and write to
               -> Excepts '[DownloadFailed] m ()
downloadToFile https host fullPath port destFile = do
  fd <- liftIO $ openFile destFile WriteMode
  let stepper = BS.hPut fd
  flip finally (liftIO $ hClose fd)
    $ reThrowAll DownloadFailed $ downloadInternal True https host fullPath port stepper


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
            | scode >= 300 && scode < 400 -> case getHeader r "Location" of
              Just r' -> pure $ Just r'
              Nothing -> throwE NoLocationHeader
            | otherwise -> throwE $ HTTPStatusError scode
        )

    followRedirectURL bs = case parseURI strictURIParserOptions bs of
      Right uri' -> do
        (https', host', fullPath', port') <- liftE $ uriToQuadruple uri'
        go (redirs - 1) progressBar https' host' fullPath' port' consumer
      Left e -> throwE e

    downloadStream r i' = do
      let size = case getHeader r "Content-Length" of
            Just x' -> case decimal $ decUTF8Safe x' of
              Left  _       -> 0
              Right (r', _) -> r'
            Nothing -> 0

      mpb <- if progressBar
        then Just <$> liftIO (newProgressBar defStyle 10 (Progress 0 size ()))
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
              , ProcessError
              ]
             m
             (M.Map (CI ByteString) ByteString)
getHead uri' | scheme == "https" = head' True
             | scheme == "http"  = head' False
             | otherwise         = throwE UnsupportedScheme

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
              pure $ Right headers
            | scode >= 300 && scode < 400 -> case getHeader r "Location" of
              Just r' -> pure $ Left r'
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
withConnection' https host port = bracket acquire closeConnection

 where
  acquire = case https of
    True -> do
      ctx <- baselineContextSSL
      openConnectionSSL ctx host (fromIntegral $ fromMaybe 443 port)
    False -> openConnection host (fromIntegral $ fromMaybe 80 port)
