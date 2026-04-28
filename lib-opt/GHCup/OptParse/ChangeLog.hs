{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHCup.OptParse.ChangeLog where


import GHCup.Errors
import GHCup.OptParse.Common
import GHCup.Prelude
import GHCup.Prelude.Process   ( exec )
import GHCup.Prelude.String.QQ
import GHCup.Query.Metadata
import GHCup.Types

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Exception.Safe         ( MonadMask )
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Char                      ( toLower )
import Data.Functor
import Data.Maybe
import GHCup.Types.Optics
import Options.Applicative            hiding ( style )
import Prelude                        hiding ( appendFile )
import System.Exit
import System.Process                 ( system )
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import URI.ByteString                 ( serializeURIRef' )

import qualified Data.Text as T



    ---------------
    --[ Options ]--
    ---------------


data ChangeLogOptions = ChangeLogOptions
  { clOpen :: Bool
  , clTool :: Maybe Tool
  , clToolVer :: Maybe ToolVersion
  }
  deriving (Eq, Show)




    ---------------
    --[ Parsers ]--
    ---------------


changelogP :: Parser ChangeLogOptions
changelogP =
  (\x y -> ChangeLogOptions x y)
    <$> switch (short 'o' <> long "open" <> help "xdg-open the changelog url")
    <*> optional
          (option
            (eitherReader
              (\s' -> Right (Tool (fmap toLower s'))
              )
            )
            (short 't' <> long "tool" <> metavar "<ghc|cabal|hls|ghcup|stack>" <> help
              "Open changelog for given tool (default: ghc)"
              <> completer toolCompleter
            )
          )
    <*> optional (toolVersionTagArgument [] Nothing)



    --------------
    --[ Footer ]--
    --------------


changeLogFooter :: String
changeLogFooter = [s|Discussion:
  By default returns the URI of the ChangeLog of the latest GHC release.
  Pass '-o' to automatically open via xdg-open.|]



    ------------------
    --[ Entrypoint ]--
    ------------------



changelog :: ( Monad m
             , MonadMask m
             , MonadUnliftIO m
             , MonadFail m
             )
          => ChangeLogOptions
          -> (forall a . ReaderT AppState m a -> m a)
          -> (ReaderT LeanAppState m () -> m ())
          -> m ExitCode
changelog ChangeLogOptions{..} runAppState runLogger = do
  GHCupInfo { _ghcupDownloads = dls } <- runAppState getGHCupInfo
  let tool = fromMaybe ghc clTool
      ver' = fromMaybe
        (ToolTag Latest)
        clToolVer
      muri = getChangeLog dls tool ver'
  case muri of
    Nothing -> do
      runLogger
        (logWarn $
          "Could not find ChangeLog for " <> T.pack (prettyShow tool) <> ", version " <> T.pack (prettyShow ver')
        )
      pure ExitSuccess
    Just uri -> do
      pfreq <- runAppState getPlatformReq
      let uri' = T.unpack . decUTF8Safe . serializeURIRef' $ uri
      if clOpen
        then do
          runAppState $
            case _rPlatform pfreq of
              Darwin  -> exec "open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
              Linux _ -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
              FreeBSD -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
              OpenBSD -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
              Windows -> do
                let args = "start \"\" " ++ T.unpack (decUTF8Safe $ serializeURIRef' uri)
                c <- liftIO $ system args
                case c of
                   (ExitFailure xi) -> pure $ Left $ NonZeroExit xi "cmd.exe" [args]
                   ExitSuccess -> pure $ Right ()
              >>= \case
                    Right _ -> pure ExitSuccess
                    Left  e -> logError (T.pack $ prettyHFError e)
                      >> pure (ExitFailure 13)
        else liftIO $ putStrLn uri' >> pure ExitSuccess

