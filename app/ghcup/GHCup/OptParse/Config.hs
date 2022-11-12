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
import           URI.ByteString          hiding ( uriParser )

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
  | AddReleaseChannel URI



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
    <|> argsP -- add show for a single option
    <|> pure ShowConfig
 where
  initP = info (pure InitConfig) (progDesc "Write default config to ~/.ghcup/config.yaml")
  showP = info (pure ShowConfig) (progDesc "Show current config (default)")
  setP  = info argsP (progDesc "Set config KEY to VALUE (or specify as single json value)" <> footerDoc (Just $ text configSetFooter))
  argsP = SetConfig <$> argument str (metavar "<JSON_VALUE | YAML_KEY>") <*> optional (argument str (metavar "YAML_VALUE"))
  addP  = info (AddReleaseChannel <$> argument (eitherReader uriParser) (metavar "URI" <> completer fileUri))
    (progDesc "Add a release channel from a URI")




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


updateSettings :: UserSettings -> Settings -> Settings
updateSettings UserSettings{..} Settings{..} =
   let cache'      = fromMaybe cache uCache
       metaCache'  = fromMaybe metaCache uMetaCache
       noVerify'   = fromMaybe noVerify uNoVerify
       keepDirs'   = fromMaybe keepDirs uKeepDirs
       downloader' = fromMaybe downloader uDownloader
       verbose'    = fromMaybe verbose uVerbose
       urlSource'  = fromMaybe urlSource uUrlSource
       noNetwork'  = fromMaybe noNetwork uNoNetwork
       gpgSetting' = fromMaybe gpgSetting uGPGSetting
       platformOverride' = uPlatformOverride <|> platformOverride
   in Settings cache' metaCache' noVerify' keepDirs' downloader' verbose' urlSource' noNetwork' gpgSetting' noColor platformOverride'



    ------------------
    --[ Entrypoint ]--
    ------------------



config :: forall m. ( Monad m
          , MonadMask m
          , MonadUnliftIO m
          , MonadFail m
          )
     => ConfigCommand
     -> Settings
     -> KeyBindings
     -> (ReaderT LeanAppState m () -> m ())
     -> m ExitCode
config configCommand settings keybindings runLogger = case configCommand of
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
          lift $ doConfig usersettings
          pure ()
        Just v -> do
          usersettings <- decodeSettings (k <> ": " <> v <> "\n")
          lift $ doConfig usersettings
          pure ()
    case r of
      VRight _ -> pure ExitSuccess
      VLeft (V (JSONDecodeError e)) -> do
        runLogger $ logError $ "Error decoding config: " <> T.pack e
        pure $ ExitFailure 65
      VLeft _ -> pure $ ExitFailure 65

  AddReleaseChannel uri -> do
    case urlSource settings of
      AddSource xs -> do
        doConfig (defaultUserSettings { uUrlSource = Just $ AddSource (xs <> [Right uri]) })
        pure ExitSuccess
      _ -> do
        doConfig (defaultUserSettings { uUrlSource = Just $ AddSource [Right uri] })
        pure ExitSuccess

 where
  doConfig :: MonadIO m => UserSettings -> m ()
  doConfig usersettings = do
    let settings' = updateSettings usersettings settings
    path <- liftIO getConfigFilePath
    liftIO $ writeFile path $ formatConfig $ fromSettings settings' (Just keybindings)
    runLogger $ logDebug $ T.pack $ show settings'
    pure ()

  decodeSettings = lE' (JSONDecodeError . displayException) . Y.decodeEither' . UTF8.fromString
