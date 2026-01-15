module GHCup.OptParse.Reset where

import GHCup.Types (UserSettings(..))

-- UserSettingsKey constructors correspond to UserSettings fields
data UserSettingsKey
    = Cache
    | MetaCache
    | MetaMode
    | NoVerify
    | Verbose
    | KeepDirs
    | Downloader
    | KeyBindings
    | UrlSource
    | NoNetwork
    | GPGSetting
    | PlatformOverride
    | Mirrors
    | DefGHCConfOptions
    | Pager
    | GuessVersion
    deriving (Show, Eq)

toUserSettingsKey :: String -> Either String UserSettingsKey
toUserSettingsKey = \case
    "cache" -> Right Cache
    "meta-cache" -> Right MetaCache
    "meta-mode" -> Right MetaMode
    "no-verify" -> Right NoVerify
    "verbose" -> Right Verbose
    "keep-dirs" -> Right KeepDirs
    "downloader" -> Right Downloader
    "key-bindings" -> Right KeyBindings
    "url-source" -> Right UrlSource
    "no-network" -> Right NoNetwork
    "gpg-setting" -> Right GPGSetting
    "platform-override" -> Right PlatformOverride
    "mirrors" -> Right Mirrors
    "def-ghc-conf-options" -> Right DefGHCConfOptions
    "pager" -> Right Pager
    "guess-version" -> Right GuessVersion
    invalidString -> Left invalidString

resetUserConfig ::
    UserSettings -> UserSettingsKey -> UserSettings
resetUserConfig settings key = case key of
    Cache -> settings { uCache = Nothing }
    MetaCache -> settings { uMetaCache = Nothing }
    MetaMode -> settings { uMetaMode = Nothing }
    NoVerify -> settings { uNoVerify = Nothing }
    Verbose -> settings { uVerbose = Nothing }
    KeepDirs -> settings { uKeepDirs = Nothing }
    Downloader -> settings { uDownloader = Nothing }
    KeyBindings -> settings { uKeyBindings = Nothing }
    UrlSource -> settings { uUrlSource = Nothing }
    NoNetwork -> settings { uNoNetwork = Nothing }
    GPGSetting -> settings { uGPGSetting = Nothing }
    PlatformOverride -> settings { uPlatformOverride = Nothing }
    Mirrors -> settings { uMirrors = Nothing }
    DefGHCConfOptions -> settings { uDefGHCConfOptions = Nothing }
    Pager -> settings { uPager = Nothing }
    GuessVersion -> settings { uGuessVersion = Nothing }


