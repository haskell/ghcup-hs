{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GHCup.OptParse.Common where


import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.Logger
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude

import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List                      ( nub, sort, sortBy )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Versions           hiding ( str )
import           Data.Void
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           Safe
import           System.FilePath
import           URI.ByteString

import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import GHCup.Version


    -------------
    --[ Types ]--
    -------------

data ToolVersion = ToolVersion GHCTargetVersion -- target is ignored for cabal
                 | ToolTag Tag

-- a superset of ToolVersion
data SetToolVersion = SetToolVersion GHCTargetVersion
                    | SetToolTag Tag
                    | SetRecommended
                    | SetNext

prettyToolVer :: ToolVersion -> String
prettyToolVer (ToolVersion v') = T.unpack $ tVerToText v'
prettyToolVer (ToolTag t) = show t

toSetToolVer :: Maybe ToolVersion -> SetToolVersion
toSetToolVer (Just (ToolVersion v')) = SetToolVersion v'
toSetToolVer (Just (ToolTag t')) = SetToolTag t'
toSetToolVer Nothing = SetRecommended




    --------------
    --[ Parser ]--
    --------------


-- | same as toolVersionParser, except as an argument.
toolVersionArgument :: Maybe ListCriteria -> Maybe Tool -> Parser ToolVersion
toolVersionArgument criteria tool =
  argument (eitherReader toolVersionEither)
    (metavar (mv tool)
    <> completer (tagCompleter (fromMaybe GHC tool) [])
    <> foldMap (completer . versionCompleter criteria) tool)
 where
  mv (Just GHC) = "GHC_VERSION|TAG"
  mv (Just HLS) = "HLS_VERSION|TAG"
  mv _          = "VERSION|TAG"


versionParser :: Parser GHCTargetVersion
versionParser = option
  (eitherReader tVersionEither)
  (short 'v' <> long "version" <> metavar "VERSION" <> help "The target version"
  )

versionParser' :: Maybe ListCriteria -> Maybe Tool -> Parser Version
versionParser' criteria tool = argument
  (eitherReader (first show . version . T.pack))
  (metavar "VERSION"  <> foldMap (completer . versionCompleter criteria) tool)

versionArgument :: Maybe ListCriteria -> Maybe Tool -> Parser GHCTargetVersion
versionArgument criteria tool = argument (eitherReader tVersionEither) (metavar "VERSION" <> foldMap (completer . versionCompleter criteria) tool)


-- https://github.com/pcapriotti/optparse-applicative/issues/148

-- | A switch that can be enabled using --foo and disabled using --no-foo.
--
-- The option modifier is applied to only the option that is *not* enabled
-- by default. For example:
--
-- > invertableSwitch "recursive" True (help "do not recurse into directories")
--
-- This example makes --recursive enabled by default, so
-- the help is shown only for --no-recursive.
invertableSwitch
    :: String              -- ^ long option
    -> Char                -- ^ short option for the non-default option
    -> Bool                -- ^ is switch enabled by default?
    -> Mod FlagFields Bool -- ^ option modifier
    -> Parser (Maybe Bool)
invertableSwitch longopt shortopt defv optmod = invertableSwitch' longopt shortopt defv
    (if defv then mempty else optmod)
    (if defv then optmod else mempty)

-- | Allows providing option modifiers for both --foo and --no-foo.
invertableSwitch'
    :: String              -- ^ long option (eg "foo")
    -> Char                -- ^ short option for the non-default option
    -> Bool                -- ^ is switch enabled by default?
    -> Mod FlagFields Bool -- ^ option modifier for --foo
    -> Mod FlagFields Bool -- ^ option modifier for --no-foo
    -> Parser (Maybe Bool)
invertableSwitch' longopt shortopt defv enmod dismod = optional
    ( flag' True ( enmod <> long longopt <> if defv then mempty else short shortopt)
    <|> flag' False (dismod <> long nolongopt <> if defv then short shortopt else mempty)
    )
  where
    nolongopt = "no-" ++ longopt



    ---------------------
    --[ Either Parser ]--
    ---------------------


platformParser :: String -> Either String PlatformRequest
platformParser s' = case MP.parse (platformP <* MP.eof) "" (T.pack s') of
  Right r -> pure r
  Left  e -> Left $ errorBundlePretty e
 where
  archP :: MP.Parsec Void Text Architecture
  archP = MP.try (MP.chunk "x86_64" $> A_64) <|> (MP.chunk "i386" $> A_32)
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
    ]
  distroP :: MP.Parsec Void Text LinuxDistro
  distroP = choice'
    [ MP.chunk "debian" $> Debian
    , MP.chunk "deb" $> Debian
    , MP.chunk "ubuntu" $> Ubuntu
    , MP.chunk "mint" $> Mint
    , MP.chunk "fedora" $> Fedora
    , MP.chunk "centos" $> CentOS
    , MP.chunk "redhat" $> RedHat
    , MP.chunk "alpine" $> Alpine
    , MP.chunk "gentoo" $> Gentoo
    , MP.chunk "exherbo" $> Exherbo
    , MP.chunk "unknown" $> UnknownLinux
    ]


bindistParser :: String -> Either String URI
bindistParser = first show . parseURI strictURIParserOptions . UTF8.fromString


absolutePathParser :: FilePath -> Either String FilePath
absolutePathParser f = case isValid f && isAbsolute f of
              True -> Right $ normalise f
              False -> Left "Please enter a valid absolute filepath."

isolateParser :: FilePath -> Either String FilePath
isolateParser f = case isValid f of
              True -> Right $ normalise f
              False -> Left "Please enter a valid filepath for isolate dir."

toolVersionEither :: String -> Either String ToolVersion
toolVersionEither s' =
  second ToolTag (tagEither s') <|> second ToolVersion (tVersionEither s')

tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended" -> Right Recommended
  "latest"      -> Right Latest
  ('b':'a':'s':'e':'-':ver') -> case pvp (T.pack ver') of
                                  Right x -> Right (Base x)
                                  Left  _ -> Left $ "Invalid PVP version for base " <> ver'
  other         -> Left $ "Unknown tag " <> other


tVersionEither :: String -> Either String GHCTargetVersion
tVersionEither =
  first (const "Not a valid version") . MP.parse ghcTargetVerP "" . T.pack


toolParser :: String -> Either String Tool
toolParser s' | t == T.pack "ghc"   = Right GHC
              | t == T.pack "cabal" = Right Cabal
              | t == T.pack "hls"   = Right HLS
              | t == T.pack "stack" = Right Stack
              | otherwise           = Left ("Unknown tool: " <> s')
  where t = T.toLower (T.pack s')


criteriaParser :: String -> Either String ListCriteria
criteriaParser s' | t == T.pack "installed" = Right ListInstalled
                  | t == T.pack "set"       = Right ListSet
                  | t == T.pack "available" = Right ListAvailable
                  | otherwise               = Left ("Unknown criteria: " <> s')
  where t = T.toLower (T.pack s')


toolVersionParser :: Parser ToolVersion
toolVersionParser = verP' <|> toolP
 where
  verP' = ToolVersion <$> versionParser
  toolP =
    ToolTag
      <$> option
            (eitherReader tagEither)
            (short 't' <> long "tag" <> metavar "TAG" <> help "The target tag")




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



    ------------------
    --[ Completers ]--
    ------------------

tagCompleter :: Tool -> [String] -> Completer
tagCompleter tool add = listIOCompleter $ do
  dirs' <- liftIO getAllDirs
  let loggerConfig = LoggerConfig
        { lcPrintDebug   = False
        , consoleOutter  = mempty
        , fileOutter     = mempty
        , fancyColors    = False
        }
  let appState = LeanAppState
        (Settings True False Never Curl False GHCupURL True GPGNone False)
        dirs'
        defaultKeyBindings
        loggerConfig

  mGhcUpInfo <- flip runReaderT appState . runE $ getDownloadsF
  case mGhcUpInfo of
    VRight ghcupInfo -> do
      let allTags = filter (/= Old)
            $ _viTags =<< M.elems (availableToolVersions (_ghcupDownloads ghcupInfo) tool)
      pure $ nub $ (add ++) $ fmap tagToString allTags
    VLeft _ -> pure  (nub $ ["recommended", "latest"] ++ add)


versionCompleter :: Maybe ListCriteria -> Tool -> Completer
versionCompleter criteria tool = listIOCompleter $ do
  dirs' <- liftIO getAllDirs
  let loggerConfig = LoggerConfig
        { lcPrintDebug   = False
        , consoleOutter  = mempty
        , fileOutter     = mempty
        , fancyColors    = False
        }
  let settings = Settings True False Never Curl False GHCupURL True GPGNone False
  let leanAppState = LeanAppState
                   settings
                   dirs'
                   defaultKeyBindings
                   loggerConfig
  mpFreq <- flip runReaderT leanAppState . runE $ platformRequest
  mGhcUpInfo <- flip runReaderT leanAppState . runE $ getDownloadsF
  forFold mpFreq $ \pfreq -> do
    forFold mGhcUpInfo $ \ghcupInfo -> do
      let appState = AppState
            settings
            dirs'
            defaultKeyBindings
            ghcupInfo
            pfreq
            loggerConfig

          runEnv = flip runReaderT appState

      installedVersions <- runEnv $ listVersions (Just tool) criteria
      return $ T.unpack . prettyVer . lVer <$> installedVersions




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
            -> Tool
            -> Excepts
                 '[ TagNotFound
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
             -> Tool
             -> Excepts
                  '[ TagNotFound
                   , NextVerNotFound
                   , NoToolVersionSet
                   ] m (GHCTargetVersion, Maybe VersionInfo)
fromVersion' SetRecommended tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  bimap mkTVer Just <$> getRecommended dls tool
    ?? TagNotFound Recommended tool
fromVersion' (SetToolVersion v) tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  let vi = getVersionInfo (_tvVersion v) tool dls
  case pvp $ prettyVer (_tvVersion v) of -- need to be strict here
    Left _ -> pure (v, vi)
    Right pvpIn ->
      lift (getLatestToolFor tool pvpIn dls) >>= \case
        Just (pvp_, vi') -> do
          v' <- lift $ pvpToVersion pvp_
          when (v' /= _tvVersion v) $ lift $ logWarn ("Assuming you meant version " <> prettyVer v')
          pure (GHCTargetVersion (_tvTarget v) v', Just vi')
        Nothing -> pure (v, vi)
fromVersion' (SetToolTag Latest) tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  bimap mkTVer Just <$> getLatest dls tool ?? TagNotFound Latest tool
fromVersion' (SetToolTag Recommended) tool = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  bimap mkTVer Just <$> getRecommended dls tool ?? TagNotFound Recommended tool
fromVersion' (SetToolTag (Base pvp'')) GHC = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  bimap mkTVer Just <$> getLatestBaseVersion dls pvp'' ?? TagNotFound (Base pvp'') GHC
fromVersion' SetNext tool = do
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
  let vi = getVersionInfo (_tvVersion next) tool dls
  pure (next, vi)
fromVersion' (SetToolTag t') tool =
  throwE $ TagNotFound t' tool


checkForUpdates :: ( MonadReader env m
                   , HasGHCupInfo env
                   , HasDirs env
                   , HasPlatformReq env
                   , MonadCatch m
                   , HasLog env
                   , MonadThrow m
                   , MonadIO m
                   , MonadFail m
                   )
                => m ()
checkForUpdates = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
  lInstalled <- listVersions Nothing (Just ListInstalled)
  let latestInstalled tool = (fmap lVer . lastMay . filter (\lr -> lTool lr == tool)) lInstalled

  forM_ (getLatest dls GHCup) $ \(l, _) -> do
    (Right ghc_ver) <- pure $ version $ prettyPVP ghcUpVer
    when (l > ghc_ver)
      $ logWarn $
          "New GHCup version available: " <> prettyVer l <> ". To upgrade, run 'ghcup upgrade'"

  forM_ (getLatest dls GHC) $ \(l, _) -> do
    let mghc_ver = latestInstalled GHC
    forM mghc_ver $ \ghc_ver ->
      when (l > ghc_ver)
        $ logWarn $
          "New GHC version available: " <> prettyVer l <> ". To upgrade, run 'ghcup install ghc " <> prettyVer l <> "'"

  forM_ (getLatest dls Cabal) $ \(l, _) -> do
    let mcabal_ver = latestInstalled Cabal
    forM mcabal_ver $ \cabal_ver ->
      when (l > cabal_ver)
        $ logWarn $
          "New Cabal version available: " <> prettyVer l <> ". To upgrade, run 'ghcup install cabal " <> prettyVer l <> "'"

  forM_ (getLatest dls HLS) $ \(l, _) -> do
    let mhls_ver = latestInstalled HLS
    forM mhls_ver $ \hls_ver ->
      when (l > hls_ver)
        $ logWarn $
          "New HLS version available: " <> prettyVer l <> ". To upgrade, run 'ghcup install hls " <> prettyVer l <> "'"

  forM_ (getLatest dls Stack) $ \(l, _) -> do
    let mstack_ver = latestInstalled Stack
    forM mstack_ver $ \stack_ver ->
      when (l > stack_ver)
        $ logWarn $
          "New Stack version available: " <> prettyVer l <> ". To upgrade, run 'ghcup install stack " <> prettyVer l <> "'"
