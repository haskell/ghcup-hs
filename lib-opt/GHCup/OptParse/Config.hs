{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Config where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.Parsers (parseNewUrlSource)
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.OptParse.Common

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Exception              ( displayException )
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit

import qualified Data.Text                     as T
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Yaml.Aeson               as Y
import Control.Exception.Safe (MonadMask)




    ----------------
    --[ Commands ]--
    ----------------


data ConfigCommand
  = ShowConfig
  | SetConfig String (Maybe String)
  | InitConfig
  | AddReleaseChannel Bool NewURLSource
  deriving (Eq, Show)



    ---------------
    --[ Parsers ]--
    ---------------


configP :: Parser ConfigCommand
configP = subparser
      (  command "init" initP
      <> command "set"  setP -- [set] KEY VALUE at help lhs
      <> command "show" showP
      <> command "add-release-channel" addP
      )
    <|> pure ShowConfig
 where
  initP = info (pure InitConfig) (progDesc "Write default config to ~/.ghcup/config.yaml")
  showP = info (pure ShowConfig) (progDesc "Show current config (default)")
  setP  = info argsP (progDesc "Set config KEY to VALUE (or specify as single json value)" <> footerDoc (Just $ text configSetFooter))
  argsP = SetConfig <$> argument str (metavar "<JSON_VALUE | YAML_KEY>") <*> optional (argument str (metavar "YAML_VALUE"))
  addP  = info (AddReleaseChannel <$> switch (long "force" <> help "Delete existing entry (if any) and append instead of failing") <*> argument (eitherReader parseNewUrlSource) (metavar "URL_SOURCE" <> completer urlSourceCompleter))
    (progDesc "Add a release channel, e.g. from a URI")




    --------------
    --[ Footer ]--
    --------------


configFooter :: String
configFooter = [s|Examples:

  # show current config
  ghcup config

  # initialize config
  ghcup config init

  # set <key> <value> configuration pair
  ghcup config set <key> <value>|]


configSetFooter :: String
configSetFooter = [s|Examples:
  # disable caching
  ghcup config set cache false

  # switch downloader to wget
  ghcup config set downloader Wget

  # set mirror for ghcup metadata
  ghcup config set '{url-source: { OwnSource: "<url>"}}'|]



    -----------------
    --[ Utilities ]--
    -----------------


formatConfig :: UserSettings -> String
formatConfig = UTF8.toString . Y.encode


updateSettings :: UserSettings -> UserSettings -> UserSettings
updateSettings usl usr =
   let cache'      = uCache usl      <|> uCache usr
       metaCache'  = uMetaCache usl  <|> uMetaCache usr
       metaMode'   = uMetaMode usl   <|> uMetaMode usr
       noVerify'   = uNoVerify usl   <|> uNoVerify usr
       verbose'    = uVerbose usl    <|> uVerbose usr
       keepDirs'   = uKeepDirs usl   <|> uKeepDirs usr
       downloader' = uDownloader usl <|> uDownloader usr
       urlSource'  = uUrlSource usl  <|> uUrlSource usr
       noNetwork'  = uNoNetwork usl  <|> uNoNetwork usr
       gpgSetting' = uGPGSetting usl <|> uGPGSetting usr
       platformOverride' = uPlatformOverride usl <|> uPlatformOverride usr
       mirrors' = uMirrors usl <|> uMirrors usr
       defGHCconfOptions' = uDefGHCConfOptions usl <|> uDefGHCConfOptions usr
   in UserSettings cache' metaCache' metaMode' noVerify' verbose' keepDirs' downloader' (updateKeyBindings (uKeyBindings usl) (uKeyBindings usr)) urlSource' noNetwork' gpgSetting' platformOverride' mirrors' defGHCconfOptions'
 where
  updateKeyBindings :: Maybe UserKeyBindings -> Maybe UserKeyBindings -> Maybe UserKeyBindings
  updateKeyBindings Nothing Nothing = Nothing
  updateKeyBindings (Just kbl) Nothing = Just kbl
  updateKeyBindings Nothing (Just kbr) = Just kbr
  updateKeyBindings (Just kbl) (Just kbr) =
       Just $ UserKeyBindings {
           kUp = kUp kbl <|> kUp kbr
         , kDown = kDown kbl <|> kDown kbr
         , kQuit = kQuit kbl <|> kQuit kbr
         , kInstall = kInstall kbl <|> kInstall kbr
         , kUninstall = kUninstall kbl <|> kUninstall kbr
         , kSet = kSet kbl <|> kSet kbr
         , kChangelog = kChangelog kbl <|> kChangelog kbr
         , kShowAll = kShowAll kbl <|> kShowAll kbr
         }



    ------------------
    --[ Entrypoint ]--
    ------------------

data Duplicate = Duplicate     -- ^ there is a duplicate somewhere in the middle
               | NoDuplicate   -- ^ there is no duplicate
               | DuplicateLast -- ^ there's a duplicate, but it's the last element


config :: forall m. ( Monad m
          , MonadMask m
          , MonadUnliftIO m
          , MonadFail m
          )
     => ConfigCommand
     -> Settings
     -> UserSettings
     -> KeyBindings
     -> (ReaderT LeanAppState m () -> m ())
     -> m ExitCode
config configCommand settings userConf keybindings runLogger = case configCommand of
  InitConfig -> do
    path <- getConfigFilePath
    liftIO $ writeFile path $ formatConfig $ fromSettings settings (Just keybindings)
    runLogger $ logDebug $ "config.yaml initialized at " <> T.pack path
    pure ExitSuccess

  ShowConfig -> do
    liftIO $ putStrLn $ formatConfig $ fromSettings settings (Just keybindings)
    pure ExitSuccess

  (SetConfig k mv) -> do
    r <- runE @'[JSONError, ParseError] $ do
      case mv of
        Just "" ->
          throwE $ ParseError "Empty values are not allowed"
        Nothing -> do
          usersettings <- decodeSettings k
          when (usersettings == defaultUserSettings)
            $ throwE $ ParseError ("Failed to parse setting (maybe typo?): " <> k)
          lift $ doConfig usersettings
          pure ()
        Just v -> do
          usersettings <- decodeSettings (k <> ": " <> v <> "\n")
          when (usersettings == defaultUserSettings)
            $ throwE $ ParseError ("Failed to parse key '" <> k <> "' with value '" <> v <> "' as user setting. Maybe typo?")
          lift $ doConfig usersettings
          pure ()
    case r of
      VRight _ -> pure ExitSuccess
      VLeft (V (JSONDecodeError e)) -> do
        runLogger $ logError $ "Error decoding config: " <> T.pack e
        pure $ ExitFailure 65
      VLeft e -> do
        runLogger (logError $ T.pack $ prettyHFError e)
        pure $ ExitFailure 65

  AddReleaseChannel force new -> do
    r <- runE @'[DuplicateReleaseChannel] $ do
      let oldSources = fromURLSource (urlSource settings)
      let merged = oldSources ++ [new]
      case checkDuplicate oldSources new of
        Duplicate
          | not force -> throwE (DuplicateReleaseChannel new)
        DuplicateLast -> pure ()
        _ -> lift $ doConfig (defaultUserSettings { uUrlSource = Just $ SimpleList merged })
    case r of
      VRight _ -> do
        pure ExitSuccess
      VLeft e -> do
        runLogger $ logError $ T.pack $ prettyHFError e
        pure $ ExitFailure 15

 where
  checkDuplicate :: Eq a => [a] -> a -> Duplicate
  checkDuplicate xs a
    | last xs == a = DuplicateLast
    | a `elem` xs  = Duplicate
    | otherwise    = NoDuplicate

  doConfig :: MonadIO m => UserSettings -> m ()
  doConfig usersettings = do
    let settings' = updateSettings usersettings userConf
    path <- liftIO getConfigFilePath
    liftIO $ writeFile path $ formatConfig $ settings'
    runLogger $ logDebug $ T.pack $ show settings'
    pure ()

  decodeSettings = lE' (JSONDecodeError . displayException) . Y.decodeEither' . UTF8.fromString
