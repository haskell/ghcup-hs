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
Resetting config uses type-checking to prevent following scenarios
either some UserSetting fields are uncovered by the key handler
or some exceeded keys are handling.
-}

module GHCup.OptParse.Reset where

import GHCup.Types (UserSettings(..))

import Data.Kind (Constraint, Type)
import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

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

deriving instance Generic Key

data KeyVal (s :: Key) where
    CacheVal :: KeyVal 'Cache
    MetaCacheVal :: KeyVal 'MetaCache
    MetaModeVal :: KeyVal 'MetaMode
    NoVerifyVal :: KeyVal 'NoVerify
    VerboseVal :: KeyVal 'Verbose
    KeepDirsVal :: KeyVal 'KeepDirs
    DownloaderVal :: KeyVal 'Downloader
    KeyBindingsVal :: KeyVal 'KeyBindings
    UrlSourceVal :: KeyVal 'UrlSource
    NoNetworkVal :: KeyVal 'NoNetwork
    GPGSettingVal :: KeyVal 'GPGSetting
    PlatformOverrideVal :: KeyVal 'PlatformOverride
    MirrorsVal :: KeyVal 'Mirrors
    DefGHCConfOptionsVal :: KeyVal 'DefGHCConfOptions
    PagerVal :: KeyVal 'Pager
    GuessVersionVal :: KeyVal 'GuessVersion

toKeyVal ::
    Key -> (forall s. (ResetField s UserSettings, HasValidKey s) => KeyVal s -> r) -> r
toKeyVal Cache k = k CacheVal
toKeyVal MetaCache k = k MetaCacheVal
toKeyVal MetaMode k = k MetaModeVal
toKeyVal NoVerify k = k NoVerifyVal
toKeyVal Verbose k = k VerboseVal
toKeyVal KeepDirs k = k KeepDirsVal
toKeyVal Downloader k = k DownloaderVal
toKeyVal KeyBindings k = k KeyBindingsVal
toKeyVal UrlSource k = k UrlSourceVal
toKeyVal NoNetwork k = k NoNetworkVal
toKeyVal GPGSetting k = k GPGSettingVal
toKeyVal PlatformOverride k = k PlatformOverrideVal
toKeyVal Mirrors k = k MirrorsVal
toKeyVal DefGHCConfOptions k = k DefGHCConfOptionsVal
toKeyVal Pager k = k PagerVal
toKeyVal GuessVersion k = k GuessVersionVal

--Links Key with fields
type family FieldNameOf (s :: Key) :: Symbol where
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
    FieldNameOf s = TypeError ('Text "Keyor " ':<>: 'ShowType s ':<>: 'Text " has no corresponding field in UserSettings")

-- Check that Key is valid
type family HasValidKey (s :: Key) :: Constraint where
    HasValidKey s =
        If
            (HasValidKey' (FieldNameOf s) (FieldNames UserSettings))
            (() :: Constraint)
            ( TypeError
                ( 'Text "Keyor "
                    ':<>: 'ShowType s
                    ':<>: 'Text " is not valid for UserSettings"
                )
            )

type family HasValidKey' (field :: Symbol) (fields :: [Symbol]) :: Bool where
    HasValidKey' field '[] = 'False
    HasValidKey' field (f ': fs) =
        If
            (field == f)
            'True
            (HasValidKey' field fs)

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
type family CheckCount (keys :: [Key]) (fields :: [Symbol]) :: Constraint where
    CheckCount '[] '[] = ()
    CheckCount (s ': ss) (f ': fs) = CheckCount ss fs
    CheckCount (s ': ss) '[] =
        TypeError
            ( 'Text "Excess keys: "
                ':<>: ShowKeys (s ': ss)
            )
    CheckCount '[] (f ': fs) =
        TypeError
            ( 'Text "Uncovered fields: "
                ':<>: ShowFields (f ': fs)
            )

type family ShowKeys (keys :: [Key]) :: ErrorMessage where
    ShowKeys '[] = 'Text ""
    ShowKeys (s ': '[]) = 'ShowType s
    ShowKeys (s ': ss) = 'ShowType s ':<>: 'Text ", " ':<>: ShowKeys ss

type family ShowFields (fields :: [Symbol]) :: ErrorMessage where
    ShowFields '[] = 'Text ""
    ShowFields (f ': '[]) = 'ShowType f
    ShowFields (f ': fs) = 'ShowType f ':<>: 'Text ", " ':<>: ShowFields fs

type family AllKeys :: [Key] where
    AllKeys = '[ 'Cache, 'MetaCache, 'MetaMode, 'NoVerify, 'Verbose, 'KeepDirs, 'Downloader, 'KeyBindings, 'UrlSource, 'NoNetwork, 'GPGSetting, 'PlatformOverride, 'Mirrors, 'DefGHCConfOptions, 'Pager, 'GuessVersion ]

class ResetField (s :: Key) a where
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
    , HasValidKey s
    , CheckCount AllKeys (FieldNames UserSettings)
    ) =>
    UserSettings -> KeyVal s -> UserSettings
resetUserConfig' userSettings _ = resetField @s userSettings

resetUserConfig ::
    CheckCount AllKeys (FieldNames UserSettings) =>
    UserSettings -> Key -> UserSettings
resetUserConfig userSettings key = toKeyVal key $ \keyVal -> resetUserConfig' userSettings keyVal

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
