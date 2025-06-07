{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module GHCup.OptParse.Reset where

import GHCup.Types (UserSettings(..))

-- Key constructors correspond to UserSettings fields
data Key
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

-- Parse a string into a Key
toKey :: String -> Maybe Key
toKey = \case
    "cache" -> Just Cache
    "meta-cache" -> Just MetaCache
    "meta-mode" -> Just MetaMode
    "no-verify" -> Just NoVerify
    "verbose" -> Just Verbose
    "keep-dirs" -> Just KeepDirs
    "downloader" -> Just Downloader
    "key-bindings" -> Just KeyBindings
    "url-source" -> Just UrlSource
    "no-network" -> Just NoNetwork
    "gpg-setting" -> Just GPGSetting
    "platform-override" -> Just PlatformOverride
    "mirrors" -> Just Mirrors
    "def-ghc-conf-options" -> Just DefGHCConfOptions
    "pager" -> Just Pager
    "guess-version" -> Just GuessVersion
    _ -> Nothing

resetUserConfig ::
    UserSettings -> Key -> UserSettings
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


