{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module GHCup.Download.IOStreams where


import           GHCup.Download.Utils
import           GHCup.Errors
import           GHCup.Types.JSON               ( )
import           GHCup.Prelude
import           GHCup.Utils.URI

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Data.CaseInsensitive           ( CI, original, mk )
import           Data.IORef
import           Data.Maybe
import           Data.Text.Read
import           Haskus.Utils.Variant.Excepts
import           Network.Http.Client     hiding ( URL )
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.ProgressBar
import           URI.ByteString          hiding (parseURI)

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as M
import qualified System.IO.Streams             as Streams





    ----------------------------
    --[ Low-level (non-curl) ]--
    ----------------------------



downloadToFile :: (MonadMask m, MonadIO m)
               => Bool             -- ^ https?
               -> ByteString       -- ^ host (e.g. "www.example.com")
               -> ByteString       -- ^ path (e.g. "/my/file") including query
               -> Maybe Int        -- ^ optional port (e.g. 3000)
               -> FilePath         -- ^ destination file to create and write to
               -> M.Map (CI ByteString) ByteString -- ^ additional headers
               -> Maybe Integer    -- ^ expected content length
               -> Excepts '[DownloadFailed, HTTPNotModified] m Response
downloadToFile https host fullPath port destFile addHeaders eCSize = do
  let stepper = BS.appendFile destFile
      setup = BS.writeFile destFile mempty
  catchAllE (\case
              (V (HTTPStatusError i headers))
                | i == 304
                , Just e <- M.lookup (mk "etag") headers -> throwE $ HTTPNotModified (decUTF8Safe e)
              v -> throwE $ DownloadFailed v
            ) $ downloadInternal True https host fullPath port stepper setup addHeaders eCSize


downloadInternal :: MonadIO m
                 => Bool        -- ^ whether to show a progress bar
                 -> Bool        -- ^ https?
                 -> ByteString  -- ^ host
                 -> ByteString  -- ^ path with query
                 -> Maybe Int   -- ^ optional port
                 -> (ByteString -> IO a)   -- ^ the consuming step function
                 -> IO a                   -- ^ setup action
                 -> M.Map (CI ByteString) ByteString -- ^ additional headers
                 -> Maybe Integer
                 -> Excepts
                      '[ HTTPStatusError
                       , URIParseError
                       , UnsupportedScheme
                       , NoLocationHeader
                       , TooManyRedirs
                       , ContentLengthError
                       ]
                      m
                      Response
downloadInternal = go (5 :: Int)

 where
  go redirs progressBar https host path port consumer setup addHeaders eCSize = do
    r <- liftIO $ withConnection' https host port action
    veitherToExcepts r >>= \case
      Right r' ->
        if redirs > 0 then followRedirectURL r' else throwE TooManyRedirs
      Left res -> pure res
   where
    action c = do
      let q = buildRequest1 $ do
                http GET path
                flip M.traverseWithKey addHeaders $ \key val -> setHeader (original key) val

      sendRequest c q emptyBody

      receiveResponse
        c
        (\r i' -> runE $ do
          let scode = getStatusCode r
          if
            | scode >= 200 && scode < 300 -> liftIO $ downloadStream r i' >> pure (Left r)
            | scode == 304 -> throwE $ HTTPStatusError scode (getHeaderMap r)
            | scode >= 300 && scode < 400 -> case getHeader r "Location" of
              Just r' -> pure $ Right r'
              Nothing -> throwE NoLocationHeader
            | otherwise -> throwE $ HTTPStatusError scode (getHeaderMap r)
        )

    followRedirectURL bs = case parseURI bs of
      Right uri' -> do
        (https', host', fullPath', port') <- liftE $ uriToQuadruple uri'
        go (redirs - 1) progressBar https' host' fullPath' port' consumer setup addHeaders eCSize
      Left e -> throwE e

    downloadStream r i' = do
      void setup
      let size = case getHeader r "Content-Length" of
            Just x' -> case decimal $ decUTF8Safe x' of
              Left  _       -> Nothing
              Right (r', _) -> Just r'
            Nothing -> Nothing

      forM_ size $ \s -> forM_ eCSize $ \es -> when (es /= s) $ throwIO (ContentLengthError Nothing (Just s) es)
      let size' = eCSize <|> size

      (mpb :: Maybe (ProgressBar ())) <- case (progressBar, size') of
        (True, Just size'') -> Just <$> newProgressBar defStyle 10 (Progress 0 (fromInteger size'') ())
        _ -> pure Nothing

      ior <- liftIO $ newIORef 0

      outStream <- liftIO $ Streams.makeOutputStream
        (\case
          Just bs -> do
            let len = BS.length bs
            forM_ mpb $ \pb -> incProgress pb len

            -- check we don't exceed size
            forM_ size' $ \s -> do
              cs <- readIORef ior
              when ((cs + toInteger len) > s) $ throwIO (ContentLengthError Nothing (Just (cs + toInteger len)) s)

            modifyIORef ior (+ toInteger len)

            void $ consumer bs
          Nothing -> pure ()
        )
      liftIO $ Streams.connect i' outStream



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
