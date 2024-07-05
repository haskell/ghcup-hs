{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns      #-}

module GHCup.OptParse.Common where


import           GHCup
import           GHCup.CabalConfig
import           GHCup.Download
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import qualified GHCup.Utils.Parsers as Parsers
import           GHCup.Prelude
import           GHCup.Prelude.Process
import           GHCup.Prelude.Logger

import           Control.DeepSeq
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key    as KM
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List                      ( nub, isPrefixOf, stripPrefix )
import           Data.Maybe
import           Data.Versions
import qualified Data.Vector      as V
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           Safe (lastMay)
import           System.Process                  ( readProcess )
import           System.FilePath
import           Text.HTML.TagSoup       hiding ( Tag )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified System.FilePath.Posix         as FP
import GHCup.Version
import Control.Exception (evaluate)

    --------------
    --[ Parser ]--
    --------------


toolVersionTagArgument :: [ListCriteria] -> Maybe Tool -> Parser ToolVersion
toolVersionTagArgument criteria tool =
  argument (eitherReader (parser tool))
    (metavar (mv tool)
    <> completer (tagCompleter (fromMaybe GHC tool) [])
    <> foldMap (completer . versionCompleter criteria) tool)
 where
  mv (Just GHC) = "GHC_VERSION|TAG|RELEASE_DATE"
  mv (Just HLS) = "HLS_VERSION|TAG|RELEASE_DATE"
  mv _          = "VERSION|TAG|RELEASE_DATE"

  parser (Just GHC) = Parsers.ghcVersionTagEither
  parser Nothing    = Parsers.ghcVersionTagEither
  parser _          = Parsers.toolVersionTagEither


versionParser' :: [ListCriteria] -> Maybe Tool -> Parser Version
versionParser' criteria tool = argument
  (eitherReader (first show . version . T.pack))
  (metavar "VERSION"  <> foldMap (completer . versionCompleter criteria) tool)

ghcVersionArgument :: [ListCriteria] -> Maybe Tool -> Parser GHCTargetVersion
ghcVersionArgument criteria tool = argument (eitherReader Parsers.ghcVersionEither)
                                            (metavar "VERSION" <> foldMap (completer . versionCompleter criteria) tool)


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
    -> Maybe Char          -- ^ short option for the non-default option
    -> Bool                -- ^ is switch enabled by default?
    -> Mod FlagFields Bool -- ^ option modifier
    -> Parser (Maybe Bool)
invertableSwitch longopt shortopt defv optmod = invertableSwitch' longopt shortopt defv
    (if defv then mempty else optmod)
    (if defv then optmod else mempty)

-- | Allows providing option modifiers for both --foo and --no-foo.
invertableSwitch'
    :: String              -- ^ long option (eg "foo")
    -> Maybe Char          -- ^ short option for the non-default option
    -> Bool                -- ^ is switch enabled by default?
    -> Mod FlagFields Bool -- ^ option modifier for --foo
    -> Mod FlagFields Bool -- ^ option modifier for --no-foo
    -> Parser (Maybe Bool)
invertableSwitch' longopt shortopt defv enmod dismod = optional
    ( flag' True ( enmod <> long longopt <> if defv then mempty else maybe mempty short shortopt)
    <|> flag' False (dismod <> long nolongopt <> if defv then maybe mempty short shortopt else mempty)
    )
  where
    nolongopt = "no-" ++ longopt



    ------------------
    --[ Completers ]--
    ------------------


toolCompleter :: Completer
toolCompleter = listCompleter ["ghc", "cabal", "hls", "stack"]

gitFileUri :: [String] -> Completer
gitFileUri add = mkCompleter $ fileUri' (["git://"] <> add)

urlSourceCompleter :: Completer
urlSourceCompleter = mkCompleter $ urlSourceCompleter' []

urlSourceCompleter' :: [String] -> String -> IO [String]
urlSourceCompleter' add str' = do
  let static = ["GHCupURL", "StackSetupURL"]
  file <- fileUri' add str'
  pure $ static ++ file

fileUri :: Completer
fileUri = mkCompleter $ fileUri' []

fileUri' :: [String] -> String -> IO [String]
fileUri' add = \case
  "" -> do
    pwd <- getCurrentDirectory
    pure $ ["https://", "http://", "file:///", "file://" <> pwd <> "/"] <> add
  xs
   | "file:///" `isPrefixOf` xs -> fmap ("file://" <>) <$>
      case stripPrefix "file://" xs of
        Nothing -> pure []
        Just r ->  do
          pwd <- getCurrentDirectory
          dirs  <- compgen "directory" r ["-S", "/"]
          files <- filter (\f -> (f <> "/") `notElem` dirs) <$> compgen "file" r []
          pure (dirs <> files <> if r `isPrefixOf` pwd then [pwd <> "/"] else [])
   | xs `isPrefixOf` "file:///" -> pure ["file:///"]
   | xs `isPrefixOf` "https://" -> pure ["https://"]
   | xs `isPrefixOf` "http://"  -> pure ["http://"]
   | otherwise -> pure []
 where
  compgen :: String -> String -> [String] -> IO [String]
  compgen action' r opts = do
    let cmd = unwords $ ["compgen", "-A", action'] <> opts <> ["--", requote r]
    result <- tryIO $ readProcess "bash" ["-c", cmd] ""
    return . lines . either (const []) id $ result

  -- | Strongly quote the string we pass to compgen.
  --
  -- We need to do this so bash doesn't expand out any ~ or other
  -- chars we want to complete on, or emit an end of line error
  -- when seeking the close to the quote.
  --
  -- NOTE: copied from https://hackage.haskell.org/package/optparse-applicative-0.17.0.0/docs/src/Options.Applicative.Builder.Completer.html#requote
  requote :: String -> String
  requote s =
    let
      -- Bash doesn't appear to allow "mixed" escaping
      -- in bash completions. So we don't have to really
      -- worry about people swapping between strong and
      -- weak quotes.
      unescaped =
        case s of
          -- It's already strongly quoted, so we
          -- can use it mostly as is, but we must
          -- ensure it's closed off at the end and
          -- there's no single quotes in the
          -- middle which might confuse bash.
          ('\'': rs) -> unescapeN rs

          -- We're weakly quoted.
          ('"': rs)  -> unescapeD rs

          -- We're not quoted at all.
          -- We need to unescape some characters like
          -- spaces and quotation marks.
          elsewise   -> unescapeU elsewise
    in
      strong unescaped

    where
      strong ss = '\'' : foldr go "'" ss
        where
          -- If there's a single quote inside the
          -- command: exit from the strong quote and
          -- emit it the quote escaped, then resume.
          go '\'' t = "'\\''" ++ t
          go h t    = h : t

      -- Unescape a strongly quoted string
      -- We have two recursive functions, as we
      -- can enter and exit the strong escaping.
      unescapeN = goX
        where
          goX ('\'' : xs) = goN xs
          goX (x : xs) = x : goX xs
          goX [] = []

          goN ('\\' : '\'' : xs) = '\'' : goN xs
          goN ('\'' : xs) = goX xs
          goN (x : xs) = x : goN xs
          goN [] = []

      -- Unescape an unquoted string
      unescapeU = goX
        where
          goX [] = []
          goX ('\\' : x : xs) = x : goX xs
          goX (x : xs) = x : goX xs

      -- Unescape a weakly quoted string
      unescapeD = goX
        where
          -- Reached an escape character
          goX ('\\' : x : xs)
            -- If it's true escapable, strip the
            -- slashes, as we're going to strong
            -- escape instead.
            | x `elem` ("$`\"\\\n" :: String) = x : goX xs
            | otherwise = '\\' : x : goX xs
          -- We've ended quoted section, so we
          -- don't recurse on goX, it's done.
          goX ('"' : xs)
            = xs
          -- Not done, but not a special character
          -- just continue the fold.
          goX (x : xs)
            = x : goX xs
          goX []
            = []


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
        (defaultSettings { noNetwork = True })
        dirs'
        defaultKeyBindings
        loggerConfig

  mpFreq <- flip runReaderT appState . runE $ platformRequest
  forFold mpFreq $ \pfreq -> do
    mGhcUpInfo <- flip runReaderT appState . runE $ getDownloadsF pfreq
    case mGhcUpInfo of
      VRight ghcupInfo -> do
        let allTags = filter (/= Old)
              $ _viTags =<< M.elems (availableToolVersions (_ghcupDownloads ghcupInfo) tool)
        pure $ nub $ (add ++) $ fmap tagToString allTags
      VLeft _ -> pure  (nub $ ["recommended", "latest", "latest-prerelease"] ++ add)

versionCompleter :: [ListCriteria] -> Tool -> Completer
versionCompleter criteria tool = versionCompleter' criteria tool (const True)

versionCompleter' :: [ListCriteria] -> Tool -> (Version -> Bool) -> Completer
versionCompleter' criteria tool filter' = listIOCompleter $ do
  dirs' <- liftIO getAllDirs
  let loggerConfig = LoggerConfig
        { lcPrintDebug   = False
        , consoleOutter  = mempty
        , fileOutter     = mempty
        , fancyColors    = False
        }
  let settings = defaultSettings { noNetwork = True }
  let leanAppState = LeanAppState
                   settings
                   dirs'
                   defaultKeyBindings
                   loggerConfig
  mpFreq <- flip runReaderT leanAppState . runE $ platformRequest
  forFold mpFreq $ \pfreq -> do
    mGhcUpInfo <- flip runReaderT leanAppState . runE $ getDownloadsF pfreq
    forFold mGhcUpInfo $ \ghcupInfo -> do
      let appState = AppState
            settings
            dirs'
            defaultKeyBindings
            ghcupInfo
            pfreq
            loggerConfig

          runEnv = flip runReaderT appState

      installedVersions <- runEnv $ listVersions (Just tool) criteria False False (Nothing, Nothing)
      return $ fmap (T.unpack . prettyVer) . filter filter' . fmap lVer $ installedVersions


toolDlCompleter :: Tool -> Completer
toolDlCompleter tool = mkCompleter $ \case
  "" -> pure (initUrl tool <> ["https://", "http://", "file:///"])
  word
    | "file://" `isPrefixOf` word -> fileUri' [] word
    -- downloads.haskell.org
    | "https://downloads.haskell.org/" `isPrefixOf` word ->
        fmap (completePrefix word) . prefixMatch (FP.takeFileName word) <$> fromHRef word

    -- github releases
    | "https://github.com/haskell/haskell-language-server/releases/download/" `isPrefixOf` word
    , let xs = splitPath word
    , (length xs == 6 && last word == '/') || (length xs == 7 && last word /= '/') ->
        fmap (\x -> completePrefix word x <> "/") . prefixMatch (FP.takeFileName word) <$> getGithubReleases "haskell" "haskell-language-server"
    | "https://github.com/commercialhaskell/stack/releases/download/" == word
    , let xs = splitPath word
    , (length xs == 6 && last word == '/') || (length xs == 7 && last word /= '/') ->
        fmap (\x -> completePrefix word x <> "/") . prefixMatch (FP.takeFileName word) <$> getGithubReleases "commercialhaskell" "stack"

    -- github release assets
    | "https://github.com/haskell/haskell-language-server/releases/download/" `isPrefixOf` word
    , let xs = splitPath word
    , (length xs == 7 && last word == '/') || length xs == 8
    , let rel = xs !! 6
    , length rel > 1 -> do
        fmap (completePrefix word) . prefixMatch (FP.takeFileName word) <$> getGithubAssets "haskell" "haskell-language-server" (init rel)
    | "https://github.com/commercialhaskell/stack/releases/download/" `isPrefixOf` word
    , let xs = splitPath word
    , (length xs == 7 && last word == '/') || length xs == 8
    , let rel = xs !! 6
    , length rel > 1 -> do
        fmap (completePrefix word) . prefixMatch (FP.takeFileName word) <$> getGithubAssets "commercialhaskell" "stack" (init rel)

    -- github
    | "https://github.com/c" `isPrefixOf` word -> pure ["https://github.com/commercialhaskell/stack/releases/download/"]
    | "https://github.com/h" `isPrefixOf` word -> pure ["https://github.com/haskell/haskell-language-server/releases/download/"]
    | "https://g" `isPrefixOf` word
    , tool == Stack -> pure ["https://github.com/commercialhaskell/stack/releases/download/"]
    | "https://g" `isPrefixOf` word
    , tool == HLS -> pure ["https://github.com/haskell/haskell-language-server/releases/download/"]

    | "https://d" `isPrefixOf` word -> pure $ filter ("https://downloads.haskell.org/" `isPrefixOf`) $ initUrl tool

    | "h" `isPrefixOf` word -> pure $ initUrl tool

    | word `isPrefixOf` "file:///" -> pure ["file:///"]
    | word `isPrefixOf` "https://" -> pure ["https://"]
    | word `isPrefixOf` "http://"  -> pure ["http://"]

    | otherwise -> pure []
 where
  initUrl :: Tool -> [String]
  initUrl GHC   = [ "https://downloads.haskell.org/~ghc/"
                  , "https://downloads.haskell.org/~ghcup/unofficial-bindists/ghc/"
                  ]
  initUrl Cabal = [ "https://downloads.haskell.org/~cabal/"
                  , "https://downloads.haskell.org/~ghcup/unofficial-bindists/cabal/"
                  ]
  initUrl GHCup = [ "https://downloads.haskell.org/~ghcup/" ]
  initUrl HLS   = [ "https://github.com/haskell/haskell-language-server/releases/download/"
                  , "https://downloads.haskell.org/~ghcup/unofficial-bindists/haskell-language-server/"
                  ]
  initUrl Stack = [ "https://github.com/commercialhaskell/stack/releases/download/"
                  , "https://downloads.haskell.org/~ghcup/unofficial-bindists/stack/"
                  ]

  completePrefix :: String -- ^ url, e.g.    'https://github.com/haskell/haskell-languag'
                 -> String -- ^ match, e.g.  'haskell-language-server'
                 -> String -- ^ result, e.g. 'https://github.com/haskell/haskell-language-server'
  completePrefix url match =
    let base = FP.takeDirectory url
        fn   = FP.takeFileName url
    in if fn `isPrefixOf` match then base <> "/" <> match else url

  prefixMatch :: String -> [String] -> [String]
  prefixMatch pref = filter (pref `isPrefixOf`)

  fromHRef :: String -> IO [String]
  fromHRef url = withCurl (FP.takeDirectory url) 2_000_000 $ \stdout ->
      pure
        . fmap (T.unpack . decUTF8Safe' . fromAttrib "href")
        . filter isTagOpen
        . filter (~== ("<a href>" :: String))
        . parseTags
        $ stdout

  withCurl :: String                      -- ^ url
           -> Int                         -- ^ delay
           -> (ByteString -> IO [String]) -- ^ callback
           -> IO [String]
  withCurl url delay cb = do
    let limit = threadDelay delay
    race limit (executeOut "curl" ["-fL", url] Nothing) >>= \case
      Right (CapturedProcess {_exitCode, _stdOut}) -> do
        case _exitCode of
          ExitSuccess ->
            (try @_ @SomeException . cb $ _stdOut) >>= \case
              Left _ ->  pure []
              Right r' -> do
                r <- try @_ @SomeException
                  . evaluate
                  . force
                  $ r'
                either (\_ -> pure []) pure r
          ExitFailure _ -> pure []
      Left _ -> pure []

  getGithubReleases :: String
                    -> String
                    -> IO [String]
  getGithubReleases owner repo = withCurl url 3_000_000 $ \stdout -> do
    Just xs <- pure $ decode' @Array stdout
    fmap V.toList $ forM xs $ \x -> do
      (Object r) <- pure x
      Just (String name) <- pure $ KM.lookup (mkval "tag_name") r
      pure $ T.unpack name
   where
    url = "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/releases"

  getGithubAssets :: String
                  -> String
                  -> String
                  -> IO [String]
  getGithubAssets owner repo tag = withCurl url 3_000_000 $ \stdout -> do
    Just xs <- pure $ decode' @Object stdout
    Just (Array assets) <- pure $ KM.lookup (mkval "assets") xs
    as <- fmap V.toList $ forM assets $ \val -> do
      (Object asset) <- pure val
      Just (String name) <- pure $ KM.lookup (mkval "name") asset
      pure $ T.unpack name
    pure as
   where
    url = "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/releases/tags/" <> tag


#if MIN_VERSION_aeson(2,0,0)
  mkval = KM.fromString
#else
  mkval = id
#endif


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
                => m [(Tool, GHCTargetVersion)]
checkForUpdates = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
  lInstalled <- listVersions Nothing [ListInstalled True] False False (Nothing, Nothing)
  let latestInstalled tool = (fmap (\lr -> GHCTargetVersion (lCross lr) (lVer lr)) . lastMay . filter (\lr -> lTool lr == tool)) lInstalled

  ghcup <- forMM (getLatest dls GHCup) $ \(GHCTargetVersion _ l, _) -> do
    (Right ghcup_ver) <- pure $ version $ prettyPVP ghcUpVer
    if (l > ghcup_ver) then pure $ Just (GHCup, mkTVer l) else pure Nothing

  otherTools <- forM [GHC, Cabal, HLS, Stack] $ \t ->
    forMM (getLatest dls t) $ \(l, _) -> do
      let mver = latestInstalled t
      forMM mver $ \ver ->
        if (l > ver) then pure $ Just (t, l) else pure Nothing

  pure $ catMaybes (ghcup:otherTools)
 where
  forMM a f = fmap join $ forM a f

logGHCPostRm :: (MonadReader env m, HasLog env, MonadIO m) => GHCTargetVersion -> m ()
logGHCPostRm ghcVer = do
  cabalStore <- liftIO $ handleIO (\_ -> if isWindows then pure "C:\\cabal\\store" else pure "~/.cabal/store or ~/.local/state/cabal/store")
    getStoreDir
  let storeGhcDir = cabalStore </> ("ghc-" <> T.unpack (prettyVer $ _tvVersion ghcVer))
  logInfo $ T.pack $ "After removing GHC you might also want to clean up your cabal store at: " <> storeGhcDir
