{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns      #-}

module GHCup.Utils.Parsers where


import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.List
import           GHCup.Utils
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Attoparsec as AP
import           GHCup.Prelude.MegaParsec as MP

import           Control.Applicative ((<|>), Alternative(..))
import           Control.Monad (forM, when)
import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Attoparsec.ByteString as AP
#if MIN_VERSION_aeson(2,0,0)
#else
import qualified Data.HashMap.Strict as KM
#endif
import           Data.Bifunctor
import           Data.Char               as C
import           Data.Either
import           Data.Functor
import           Data.List                      ( sort, sortBy )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( Day )
import           Data.Time.Format               ( parseTimeM, defaultTimeLocale )
import           Data.Versions
import           Data.Void
import           Data.Variant.Excepts
import           Optics                  hiding ( set )
import           Prelude                 hiding ( appendFile )
import           Safe
import           System.FilePath
import           URI.ByteString          hiding (parseURI)

import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Encoding       as LE
import qualified Data.Text.Lazy                as LT
import qualified Text.Megaparsec               as MP
import GHCup.Version

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XQuasiQuotes
-- >>> import System.Directory
-- >>> import URI.ByteString
-- >>> import qualified Data.Text as T
-- >>> import GHCup.Prelude
-- >>> import GHCup.Download
-- >>> import GHCup.Version
-- >>> import GHCup.Errors
-- >>> import GHCup.Types
-- >>> import GHCup.Utils.Dirs
-- >>> import GHCup.Types.Optics
-- >>> import Data.Versions
-- >>> import Optics
-- >>> import GHCup.Prelude.Version.QQ
-- >>> import qualified Data.Text.Encoding as E
-- >>> import qualified Data.Map.Strict               as M
-- >>> import Control.Monad.Reader
-- >>> import Data.Variant.Excepts
-- >>> import Text.PrettyPrint.HughesPJClass ( prettyShow )
-- >>> let lc = LoggerConfig { lcPrintDebug = False, consoleOutter = mempty, fileOutter = mempty, fancyColors = False }
-- >>> dirs' <- getAllDirs
-- >>> let installedVersions = [ ([pver|8.10.7|], "-debug+lol", Nothing), ([pver|8.10.4|], "", Nothing), ([pver|8.8.4|], "", Nothing), ([pver|8.8.3|], "", Nothing) ]
-- >>> let settings = defaultSettings { cache = True, metaCache = 0, noNetwork = True }
-- >>> let leanAppState = LeanAppState settings dirs' defaultKeyBindings lc
-- >>> cwd <- getCurrentDirectory
-- >>> (Right ref) <- pure $ GHCup.Utils.parseURI $ "file://" <> E.encodeUtf8 (T.pack cwd) <> "/data/metadata/" <> (urlBaseName . view pathL' $ ghcupURL)
-- >>> (Right ref') <- pure $ GHCup.Utils.parseURI $ "file://" <> E.encodeUtf8 (T.pack cwd) <> "/data/metadata/" <> (urlBaseName . view pathL' $ channelURL PrereleasesChannel)
-- >>> (VRight r) <- (fmap . fmap) _ghcupDownloads $ flip runReaderT leanAppState . runE @'[DigestError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, ContentLengthError] $ liftE (getBase ref) >>= liftE . decodeMetadata @GHCupInfo
-- >>> (VRight r') <- (fmap . fmap) _ghcupDownloads $ flip runReaderT leanAppState . runE @'[DigestError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, ContentLengthError] $ liftE (getBase ref') >>= liftE . decodeMetadata @GHCupInfo
-- >>> let rr = M.unionsWith (M.unionWith (\_ b2 -> b2)) [r, r']
-- >>> let go = flip runReaderT leanAppState . fmap (tVerToText . fst)


    -------------
    --[ Types ]--
    -------------

-- a superset of ToolVersion
data SetToolVersion = SetGHCVersion GHCTargetVersion
                    | SetToolVersion Version
                    | SetToolTag Tag
                    | SetToolDay Day
                    | SetRecommended
                    | SetNext
                    deriving (Eq, Show)

prettyToolVer :: ToolVersion -> String
prettyToolVer (GHCVersion v')  = T.unpack $ tVerToText v'
prettyToolVer (ToolVersion v') = T.unpack $ prettyVer v'
prettyToolVer (ToolTag t)      = show t
prettyToolVer (ToolDay day)    = show day

toSetToolVer :: Maybe ToolVersion -> SetToolVersion
toSetToolVer (Just (GHCVersion v')) = SetGHCVersion v'
toSetToolVer (Just (ToolVersion v')) = SetToolVersion v'
toSetToolVer (Just (ToolTag t')) = SetToolTag t'
toSetToolVer (Just (ToolDay d')) = SetToolDay d'
toSetToolVer Nothing = SetRecommended


platformParser :: String -> Either String PlatformRequest
platformParser s' = case MP.parse (platformP <* MP.eof) "" (T.pack s') of
  Right r -> pure r
  Left  e -> Left $ errorBundlePretty e
 where
  archP :: MP.Parsec Void Text Architecture
  archP = choice' ((\x -> MP.chunk (T.pack $ archToString x) $> x) <$> ([minBound..maxBound] :: [Architecture]))
  platformP :: MP.Parsec Void Text PlatformRequest
  platformP = choice'
    [ (`PlatformRequest` FreeBSD)
    <$> (archP <* MP.chunk "-")
    <*> (  MP.chunk "portbld"
        *> (   MP.try (Just <$> verP (MP.chunk "-freebsd" <* MP.eof))
           <|> pure Nothing
           )
        <* MP.chunk "-freebsd"
        )
    , (`PlatformRequest` Darwin)
    <$> (archP <* MP.chunk "-")
    <*> (  MP.chunk "apple"
        *> (   MP.try (Just <$> verP (MP.chunk "-darwin" <* MP.eof))
           <|> pure Nothing
           )
        <* MP.chunk "-darwin"
        )
    , (\a d mv -> PlatformRequest a (Linux d) mv)
    <$> (archP <* MP.chunk "-")
    <*> distroP
    <*> ((MP.try (Just <$> verP (MP.chunk "-linux" <* MP.eof)) <|> pure Nothing
         )
        <* MP.chunk "-linux"
        )
    , (\a -> PlatformRequest a Windows Nothing)
    <$> ((archP <* MP.chunk "-")
        <* (MP.chunk "unknown-mingw32" <|> MP.chunk "unknown-windows" <|> MP.chunk "windows"))
    ]
  distroP :: MP.Parsec Void Text LinuxDistro
  distroP = choice' ((\d -> MP.chunk (T.pack $ distroToString d) $> d) <$> allDistros)


uriParser :: String -> Either String URI
uriParser = first show . parseURI . UTF8.fromString


absolutePathParser :: FilePath -> Either String FilePath
absolutePathParser f = case isValid f && isAbsolute f of
              True -> Right $ normalise f
              False -> Left "Please enter a valid absolute filepath."

isolateParser :: FilePath -> Either String FilePath
isolateParser f = case isValid f && isAbsolute f of
              True -> Right $ normalise f
              False -> Left "Please enter a valid filepath for isolate dir."

-- this accepts cross prefix
ghcVersionTagEither :: String -> Either String ToolVersion
ghcVersionTagEither s' =
  second ToolDay (dayParser s') <|> second ToolTag (tagEither s') <|> second GHCVersion (ghcVersionEither s')

-- this ignores cross prefix
toolVersionTagEither :: String -> Either String ToolVersion
toolVersionTagEither s' =
  second ToolDay (dayParser s') <|> second ToolTag (tagEither s') <|> second ToolVersion (toolVersionEither s')

tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended"              -> Right Recommended
  "latest"                   -> Right Latest
  "latest-prerelease"        -> Right LatestPrerelease
  "latest-nightly"           -> Right LatestNightly
  ('b':'a':'s':'e':'-':ver') -> case pvp (T.pack ver') of
                                  Right x -> Right (Base x)
                                  Left  _ -> Left $ "Invalid PVP version for base " <> ver'
  other                      -> Left $ "Unknown tag " <> other


ghcVersionEither :: String -> Either String GHCTargetVersion
ghcVersionEither =
  first (const "Not a valid version") . MP.parse ghcTargetVerP "" . T.pack

toolVersionEither :: String -> Either String Version
toolVersionEither =
  first (const "Not a valid version") . MP.parse (version' <* MP.eof) "" . T.pack


toolParser :: String -> Either String Tool
toolParser s' | t == T.pack "ghc"   = Right GHC
              | t == T.pack "cabal" = Right Cabal
              | t == T.pack "hls"   = Right HLS
              | t == T.pack "stack" = Right Stack
              | otherwise           = Left ("Unknown tool: " <> s')
  where t = T.toLower (T.pack s')

dayParser :: String -> Either String Day
dayParser s = maybe (Left $ "Could not parse \"" <> s <> "\". Expected format is: YYYY-MM-DD") Right
            $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" s


criteriaParser :: String -> Either String ListCriteria
criteriaParser s' | t == T.pack "installed"   = Right $ ListInstalled True
                  | t == T.pack "set"         = Right $ ListSet True
                  | t == T.pack "available"   = Right $ ListAvailable True
                  | t == T.pack "+installed"  = Right $ ListInstalled True
                  | t == T.pack "+set"        = Right $ ListSet True
                  | t == T.pack "+available"  = Right $ ListAvailable True
                  | t == T.pack "-installed"  = Right $ ListInstalled False
                  | t == T.pack "-set"        = Right $ ListSet False
                  | t == T.pack "-available"  = Right $ ListAvailable False
                  | otherwise                 = Left ("Unknown criteria: " <> s')
  where t = T.toLower (T.pack s')



keepOnParser :: String -> Either String KeepDirs
keepOnParser s' | t == T.pack "always" = Right Always
                | t == T.pack "errors" = Right Errors
                | t == T.pack "never"  = Right Never
                | otherwise            = Left ("Unknown keep value: " <> s')
  where t = T.toLower (T.pack s')


downloaderParser :: String -> Either String Downloader
downloaderParser s' | t == T.pack "curl"     = Right Curl
                    | t == T.pack "wget"     = Right Wget
#if defined(INTERNAL_DOWNLOADER)
                    | t == T.pack "internal" = Right Internal
#endif
                    | otherwise = Left ("Unknown downloader value: " <> s')
  where t = T.toLower (T.pack s')

gpgParser :: String -> Either String GPGSetting
gpgParser s' | t == T.pack "strict" = Right GPGStrict
             | t == T.pack "lax"    = Right GPGLax
             | t == T.pack "none"   = Right GPGNone
             | otherwise = Left ("Unknown gpg setting value: " <> s')
  where t = T.toLower (T.pack s')



overWriteVersionParser :: String -> Either String [VersionPattern]
overWriteVersionParser = first (const "Not a valid version pattern") . MP.parse (MP.many versionPattern <* MP.eof) "" . T.pack
 where
  versionPattern :: MP.Parsec Void Text VersionPattern
  versionPattern = do
    str' <- T.unpack <$> MP.takeWhileP Nothing (/= '%')
    if str' /= mempty
    then pure (S str')
    else     fmap (const CabalVer)      v_cabal
         <|> fmap (const GitBranchName) b_name
         <|> fmap (const GitHashShort)  s_hash
         <|> fmap (const GitHashLong)   l_hash
         <|> fmap (const GitDescribe)   g_desc
         <|> ((\a b -> S (a : T.unpack b)) <$> MP.satisfy (const True) <*> MP.takeWhileP Nothing (== '%')) -- invalid pattern, e.g. "%k"
   where
    v_cabal = MP.chunk "%v"
    b_name  = MP.chunk "%b"
    s_hash  = MP.chunk "%h"
    l_hash  = MP.chunk "%H"
    g_desc  = MP.chunk "%g"

    -----------------
    --[ Utilities ]--
    -----------------


fromVersion :: ( HasLog env
               , MonadFail m
               , MonadReader env m
               , HasGHCupInfo env
               , HasDirs env
               , MonadThrow m
               , MonadIO m
               , MonadCatch m
               )
            => Maybe ToolVersion
            -> GuessMode
            -> Tool
            -> Excepts
                 '[ TagNotFound
                  , DayNotFound
                  , NextVerNotFound
                  , NoToolVersionSet
                  ] m (GHCTargetVersion, Maybe VersionInfo)
fromVersion tv = fromVersion' (toSetToolVer tv)

fromVersion' :: ( HasLog env
                , MonadFail m
                , MonadReader env m
                , HasGHCupInfo env
                , HasDirs env
                , MonadThrow m
                , MonadIO m
                , MonadCatch m
                )
             => SetToolVersion
             -> GuessMode
             -> Tool
             -> Excepts
                  '[ TagNotFound
                   , DayNotFound
                   , NextVerNotFound
                   , NoToolVersionSet
                   ] m (GHCTargetVersion, Maybe VersionInfo)
fromVersion' SetRecommended _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getRecommended dls tool
    ?? TagNotFound Recommended tool
fromVersion' (SetGHCVersion v) guessMode tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  lift $ guessFullVersion dls v tool guessMode
fromVersion' (SetToolVersion (mkTVer -> v)) guessMode tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  lift $ guessFullVersion dls v tool guessMode
fromVersion' (SetToolTag Latest) _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getLatest dls tool ?? TagNotFound Latest tool
fromVersion' (SetToolDay day) _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> case getByReleaseDay dls tool day of
                          Left ad -> throwE $ DayNotFound day tool ad
                          Right v -> pure v
fromVersion' (SetToolTag LatestPrerelease) _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getLatestPrerelease dls tool ?? TagNotFound LatestPrerelease tool
fromVersion' (SetToolTag LatestNightly) _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getLatestNightly dls tool ?? TagNotFound LatestNightly tool
fromVersion' (SetToolTag Recommended) _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getRecommended dls tool ?? TagNotFound Recommended tool
fromVersion' (SetToolTag (Base pvp'')) _ GHC = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  second Just <$> getLatestBaseVersion dls pvp'' ?? TagNotFound (Base pvp'') GHC
fromVersion' SetNext _ tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  next <- case tool of
    GHC -> do
      set <- fmap _tvVersion $ ghcSet Nothing !? NoToolVersionSet tool
      ghcs <- rights <$> lift getInstalledGHCs
      (headMay
        . tail
        . dropWhile (\GHCTargetVersion {..} -> _tvVersion /= set)
        . cycle
        . sortBy (\x y -> compare (_tvVersion x) (_tvVersion y))
        . filter (\GHCTargetVersion {..} -> isNothing _tvTarget)
        $ ghcs) ?? NoToolVersionSet tool
    Cabal -> do
      set <- cabalSet !? NoToolVersionSet tool
      cabals <- rights <$> lift getInstalledCabals
      (fmap (GHCTargetVersion Nothing)
        . headMay
        . tail
        . dropWhile (/= set)
        . cycle
        . sort
        $ cabals) ?? NoToolVersionSet tool
    HLS -> do
      set <- hlsSet !? NoToolVersionSet tool
      hlses <- rights <$> lift getInstalledHLSs
      (fmap (GHCTargetVersion Nothing)
        . headMay
        . tail
        . dropWhile (/= set)
        . cycle
        . sort
        $ hlses) ?? NoToolVersionSet tool
    Stack -> do
      set <- stackSet !? NoToolVersionSet tool
      stacks <- rights <$> lift getInstalledStacks
      (fmap (GHCTargetVersion Nothing)
        . headMay
        . tail
        . dropWhile (/= set)
        . cycle
        . sort
        $ stacks) ?? NoToolVersionSet tool
    GHCup -> fail "GHCup cannot be set"
  let vi = getVersionInfo next tool dls
  pure (next, vi)
fromVersion' (SetToolTag t') _ tool =
  throwE $ TagNotFound t' tool

-- | Guess the full version from an input version, by possibly
-- examining the metadata and the installed versions.
--
-- >>> go $ guessFullVersion rr (mkTVer [vver|8|]) GHC GLax
-- "8.10.7"
-- >>> go $ guessFullVersion rr (mkTVer [vver|8.10|]) GHC GLax
-- "8.10.7"
-- >>> go $ guessFullVersion rr (mkTVer [vver|8.10.7|]) GHC GLax
-- "8.10.7"
-- >>> go $ guessFullVersion rr (mkTVer [vver|9.12.1|]) GHC GLax
-- "9.12.1"
-- >>> go $ guessFullVersion rr (mkTVer [vver|8|]) GHC GStrict
-- "8"
guessFullVersion :: ( HasLog env
                    , MonadFail m
                    , MonadReader env m
                    , HasDirs env
                    , MonadThrow m
                    , MonadIO m
                    , MonadCatch m
                    )
                 => GHCupDownloads
                 -> GHCTargetVersion
                 -> Tool
                 -> GuessMode
                 -> m (GHCTargetVersion, Maybe VersionInfo)
guessFullVersion dls v tool guessMode = do
  let vi = getVersionInfo v tool dls
  case pvp $ prettyVer (_tvVersion v) of -- need to be strict here
    Left _ -> pure (v, vi)
    Right pvpIn
      | (guessMode /= GStrict) && hasn't (ix tool % ix v) dls -> do
          ghcs <- if guessMode == GLaxWithInstalled then fmap rights getInstalledTools else pure []
          if v `notElem` ghcs
          then getLatestToolFor tool (_tvTarget v) pvpIn dls >>= \case
                 Just (pvp_, vi', mt) -> do
                   v' <- pvpToVersion pvp_ ""
                   when (v' /= _tvVersion v) $ logWarn ("Assuming you meant version " <> prettyVer v')
                   pure (GHCTargetVersion mt v', Just vi')
                 Nothing -> pure (v, vi)
          else pure (v, vi)
    _ -> pure (v, vi)
 where
  getInstalledTools = case tool of
                        GHC -> getInstalledGHCs
                        Cabal -> (fmap . fmap) mkTVer <$> getInstalledCabals
                        HLS -> (fmap . fmap) mkTVer <$> getInstalledHLSs
                        Stack -> (fmap . fmap) mkTVer <$> getInstalledStacks
                        GHCup -> pure []


parseUrlSource :: String -> Either String [NewURLSource]
parseUrlSource s = (fromURLSource <$> parseUrlSource' s)
               <|> ((:[]) <$> parseNewUrlSource s)
               <|> parseNewUrlSources s

parseUrlSource' :: String -> Either String URLSource
parseUrlSource' "GHCupURL" = pure GHCupURL
parseUrlSource' "StackSetupURL" = pure StackSetupURL
parseUrlSource' s' = (eitherDecode . LE.encodeUtf8 . LT.pack $ s')
            <|> (fmap (OwnSource . (:[]) . Right) . first show . parseURI . UTF8.fromString $ s')

parseNewUrlSource :: String -> Either String NewURLSource
parseNewUrlSource "GHCupURL" = pure NewGHCupURL
parseNewUrlSource "StackSetupURL" = pure NewStackSetupURL
parseNewUrlSource s' = (fmap NewChannelAlias . parseChannelAlias $ s')
            <|> (eitherDecode . LE.encodeUtf8 . LT.pack $ s')
            <|> (fmap NewURI . first show . parseURI . UTF8.fromString $ s')

parseNewUrlSources :: String -> Either String [NewURLSource]
parseNewUrlSources s = case AP.parseOnly
                              (AP.parseList' <* AP.skipSpaces <* AP.endOfInput)
                              (UTF8.fromString s) of
  Right bs ->
    forM bs $ \b -> AP.parseOnly (parse <* AP.skipSpaces <* AP.endOfInput) b
  Left  e -> Left e
 where
  parse :: AP.Parser NewURLSource
  parse = (NewGHCupURL <$ AP.string "GHCupURL")
      <|> (NewStackSetupURL <$ AP.string "StackSetupURL")
      <|> AP.choice ((\x -> AP.string (UTF8.fromString . T.unpack . channelAliasText $ x) $> NewChannelAlias x) <$> ([minBound..maxBound] :: [ChannelAlias]))
      <|> (NewURI <$> parseURIP)

parseChannelAlias :: String -> Either String ChannelAlias
parseChannelAlias s =
  let aliases = map (\c -> (T.unpack (channelAliasText c), c)) [minBound..maxBound]
  in case lookup s aliases of
    Just c -> Right c
    Nothing -> Left $ "Unexpected ChannelAlias: " <> s

#if MIN_VERSION_transformers(0,6,0)
instance Alternative (Either [a]) where
    empty        = Left []
    Left _ <|> n = n
    m      <|> _ = m
#endif

