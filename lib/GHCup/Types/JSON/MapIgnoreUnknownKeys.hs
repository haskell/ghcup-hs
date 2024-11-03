{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances #-}

module GHCup.Types.JSON.MapIgnoreUnknownKeys where

import           GHCup.Types

import           Data.Aeson              hiding (Key)
import           Data.Aeson.Types        hiding (Key)

import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.KeyMap             as KeyMap
import qualified Data.Map.Strict               as Map

#if defined(STRICT_METADATA_PARSING)
-- | Use the instance of Map
instance (FromJSON (Map.Map k v)) => FromJSON (MapIgnoreUnknownKeys k v) where
  parseJSON = fmap MapIgnoreUnknownKeys . parseJSON
#else

-- | Create a Map ignoring KeyValue pair which fail at parse of the key
-- But if the key is parsed, the failures of parsing the value will not be ignored
instance (Ord k, FromJSONKey k, FromJSON v) => FromJSON (MapIgnoreUnknownKeys k v) where
  parseJSON = withObject "MapIgnoreUnknownKeys" $ \obj -> do
    m <- case fromJSONKey of
      FromJSONKeyTextParser f ->
        let doParse k v m = case parseMaybe f (Key.toText k) of
              Just k' -> Map.insert k' <$> parseJSON v <*> m
              Nothing -> m
        in KeyMap.foldrWithKey doParse (pure Map.empty) obj
      FromJSONKeyValue f ->
        let doParse k v m = case parseMaybe f (toJSON k) of
              Just k' -> Map.insert k' <$> parseJSON v <*> m
              Nothing -> m
        in KeyMap.foldrWithKey doParse (pure Map.empty) obj
      -- FromJSONKeyCoerce and FromJSONKeyText always parse to Success; hence use instance of Map
      _ -> parseJSON (Object obj)
    pure $ MapIgnoreUnknownKeys m
#endif

instance (ToJSON (Map.Map k v)) => ToJSON (MapIgnoreUnknownKeys k v) where
  toJSON = toJSON . unMapIgnoreUnknownKeys
