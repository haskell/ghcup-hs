{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

{-
Reset scenario uses type-checking to prevent a scenario
when new field is added to UserSettings and
correspondent constructor is missed.

Type-checker signals when correspondent constructor is missed or
when not-correspondent constructor is added
-}

module GHCup.OptParse.Reset where

import GHCup.Types (UserSettings(..))

import Data.Kind (Constraint, Type)
import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

-- Select constructors correspond to UserSettings fields
data Select
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

deriving instance Generic Select

data SelectVal (s :: Select) where
    CacheVal :: SelectVal 'Cache
    MetaCacheVal :: SelectVal 'MetaCache
    MetaModeVal :: SelectVal 'MetaMode
    NoVerifyVal :: SelectVal 'NoVerify
    VerboseVal :: SelectVal 'Verbose
    KeepDirsVal :: SelectVal 'KeepDirs
    DownloaderVal :: SelectVal 'Downloader
    KeyBindingsVal :: SelectVal 'KeyBindings
    UrlSourceVal :: SelectVal 'UrlSource
    NoNetworkVal :: SelectVal 'NoNetwork
    GPGSettingVal :: SelectVal 'GPGSetting
    PlatformOverrideVal :: SelectVal 'PlatformOverride
    MirrorsVal :: SelectVal 'Mirrors
    DefGHCConfOptionsVal :: SelectVal 'DefGHCConfOptions
    PagerVal :: SelectVal 'Pager
    GuessVersionVal :: SelectVal 'GuessVersion

toSelectVal ::
    Select -> (forall s. (ResetField s UserSettings, HasValidSelector s) => SelectVal s -> r) -> r
toSelectVal Cache k = k CacheVal
toSelectVal MetaCache k = k MetaCacheVal
toSelectVal MetaMode k = k MetaModeVal
toSelectVal NoVerify k = k NoVerifyVal
toSelectVal Verbose k = k VerboseVal
toSelectVal KeepDirs k = k KeepDirsVal
toSelectVal Downloader k = k DownloaderVal
toSelectVal KeyBindings k = k KeyBindingsVal
toSelectVal UrlSource k = k UrlSourceVal
toSelectVal NoNetwork k = k NoNetworkVal
toSelectVal GPGSetting k = k GPGSettingVal
toSelectVal PlatformOverride k = k PlatformOverrideVal
toSelectVal Mirrors k = k MirrorsVal
toSelectVal DefGHCConfOptions k = k DefGHCConfOptionsVal
toSelectVal Pager k = k PagerVal
toSelectVal GuessVersion k = k GuessVersionVal

--Links Select with fields
type family FieldNameOf (s :: Select) :: Symbol where
    FieldNameOf 'Cache = "uCache"
    FieldNameOf 'MetaCache = "uMetaCache"
    FieldNameOf 'MetaMode = "uMetaMode"
    FieldNameOf 'NoVerify = "uNoVerify"
    FieldNameOf 'Verbose = "uVerbose"
    FieldNameOf 'KeepDirs = "uKeepDirs"
    FieldNameOf 'Downloader = "uDownloader"
    FieldNameOf 'KeyBindings = "uKeyBindings"
    FieldNameOf 'UrlSource = "uUrlSource"
    FieldNameOf 'NoNetwork = "uNoNetwork"
    FieldNameOf 'GPGSetting = "uGPGSetting"
    FieldNameOf 'PlatformOverride = "uPlatformOverride"
    FieldNameOf 'Mirrors = "uMirrors"
    FieldNameOf 'DefGHCConfOptions = "uDefGHCConfOptions"
    FieldNameOf 'Pager = "uPager"
    FieldNameOf 'GuessVersion = "uGuessVersion"
    FieldNameOf s = TypeError ('Text "Selector " ':<>: 'ShowType s ':<>: 'Text " has no corresponding field in UserSettings")

-- Check that Select is valid
type family HasValidSelector (s :: Select) :: Constraint where
    HasValidSelector s =
        If
            (HasValidSelector' (FieldNameOf s) (FieldNames UserSettings))
            (() :: Constraint)
            ( TypeError
                ( 'Text "Selector "
                    ':<>: 'ShowType s
                    ':<>: 'Text " is not valid for UserSettings"
                )
            )

type family HasValidSelector' (field :: Symbol) (fields :: [Symbol]) :: Bool where
    HasValidSelector' field '[] = 'False
    HasValidSelector' field (f ': fs) =
        If
            (field == f)
            'True
            (HasValidSelector' field fs)

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- Extract field names from UserSettings
type family FieldNames (r :: Type) :: [Symbol] where
    FieldNames r = FieldNames' (Rep r)

type family FieldNames' (rep :: Type -> Type) :: [Symbol] where
    FieldNames' (D1 _ (C1 _ fields)) = FieldNames'' fields
    FieldNames' (M1 _ _ fields) = FieldNames' fields

type family FieldNames'' (fields :: Type -> Type) :: [Symbol] where
    FieldNames'' (S1 (MetaSel (Just fname) _ _ _) _ :*: rest) = fname ': FieldNames'' rest
    FieldNames'' (S1 (MetaSel (Just fname) _ _ _) _) = '[fname]
    FieldNames'' (left :*: right) = FieldNames'' left ++ FieldNames'' right
    FieldNames'' U1 = '[]

-- Check correspondence by number
type family CheckCount (selectors :: [Select]) (fields :: [Symbol]) :: Constraint where
    CheckCount '[] '[] = ()
    CheckCount (s ': ss) (f ': fs) = CheckCount ss fs
    CheckCount (s ': ss) '[] =
        TypeError
            ( 'Text "Excess selectors: "
                ':<>: ShowSelectors (s ': ss)
            )
    CheckCount '[] (f ': fs) =
        TypeError
            ( 'Text "Uncovered fields: "
                ':<>: ShowFields (f ': fs)
            )

type family ShowSelectors (selectors :: [Select]) :: ErrorMessage where
    ShowSelectors '[] = 'Text ""
    ShowSelectors (s ': '[]) = 'ShowType s
    ShowSelectors (s ': ss) = 'ShowType s ':<>: 'Text ", " ':<>: ShowSelectors ss

type family ShowFields (fields :: [Symbol]) :: ErrorMessage where
    ShowFields '[] = 'Text ""
    ShowFields (f ': '[]) = 'ShowType f
    ShowFields (f ': fs) = 'ShowType f ':<>: 'Text ", " ':<>: ShowFields fs

type family AllSelectors :: [Select] where
    AllSelectors = '[ 'Cache, 'MetaCache, 'MetaMode, 'NoVerify, 'Verbose, 'KeepDirs, 'Downloader, 'KeyBindings, 'UrlSource, 'NoNetwork, 'GPGSetting, 'PlatformOverride, 'Mirrors, 'DefGHCConfOptions, 'Pager, 'GuessVersion ]

class ResetField (s :: Select) a where
    resetField :: a -> a

instance ResetField 'Cache UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uCache = Nothing }

instance ResetField 'MetaCache UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uMetaCache = Nothing }

instance ResetField 'MetaMode UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uMetaMode = Nothing }

instance ResetField 'NoVerify UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uNoVerify = Nothing }

instance ResetField 'Verbose UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uVerbose = Nothing }

instance ResetField 'KeepDirs UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uKeepDirs = Nothing }

instance ResetField 'Downloader UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uDownloader = Nothing }

instance ResetField 'KeyBindings UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uKeyBindings = Nothing }

instance ResetField 'UrlSource UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uUrlSource = Nothing }

instance ResetField 'NoNetwork UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uNoNetwork = Nothing }

instance ResetField 'GPGSetting UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uGPGSetting = Nothing }

instance ResetField 'PlatformOverride UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uPlatformOverride = Nothing }

instance ResetField 'Mirrors UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uMirrors = Nothing }

instance ResetField 'DefGHCConfOptions UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uDefGHCConfOptions = Nothing }

instance ResetField 'Pager UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uPager = Nothing }

instance ResetField 'GuessVersion UserSettings where
    resetField :: UserSettings -> UserSettings
    resetField settings = settings { uGuessVersion = Nothing }

resetUserConfig' ::
    forall s.
    ( ResetField s UserSettings
    , HasValidSelector s
    , CheckCount AllSelectors (FieldNames UserSettings)
    ) =>
    UserSettings -> SelectVal s -> UserSettings
resetUserConfig' userSettings _ = resetField @s userSettings

resetUserConfig ::
    CheckCount AllSelectors (FieldNames UserSettings) =>
    UserSettings -> Select -> UserSettings
resetUserConfig userSettings select = toSelectVal select $ \selectVal -> resetUserConfig' userSettings selectVal

toSelect :: String -> Maybe Select
toSelect = \case
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
