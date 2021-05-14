{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

#if defined(BRICK)
import           BrickMain                    ( brickMain )
#endif

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Requirements
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Logger
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ
import           GHCup.Version

#if !defined(TAR)
import           Codec.Archive
#endif
import           Control.Concurrent
import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List                      ( intercalate, nub, sort, sortBy )
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions           hiding ( str )
import           Data.Void
import           GHC.IO.Encoding
import           Haskus.Utils.Variant.Excepts
import           Language.Haskell.TH
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           Safe
import           System.Console.Pretty   hiding ( color )
import qualified System.Console.Pretty         as Pretty
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO               hiding ( appendFile )
import           Text.Read               hiding ( lift )
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           URI.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC





data Options = Options
  {
  -- global options
    optVerbose   :: Maybe Bool
  , optCache     :: Maybe Bool
  , optUrlSource :: Maybe URI
  , optNoVerify  :: Maybe Bool
  , optKeepDirs  :: Maybe KeepDirs
  , optsDownloader :: Maybe Downloader
  -- commands
  , optCommand   :: Command
  }

data Command
  = Install (Either InstallCommand InstallOptions)
  | InstallCabalLegacy InstallOptions
  | Set (Either SetCommand SetOptions)
  | List ListOptions
  | Rm (Either RmCommand RmOptions)
  | DInfo
  | Compile CompileCommand
  | Upgrade UpgradeOpts Bool
  | ToolRequirements
  | ChangeLog ChangeLogOptions
#if defined(BRICK)
  | Interactive
#endif

data ToolVersion = ToolVersion GHCTargetVersion -- target is ignored for cabal
                 | ToolTag Tag

prettyToolVer :: ToolVersion -> String
prettyToolVer (ToolVersion v') = T.unpack $ tVerToText v'
prettyToolVer (ToolTag t) = show t

toSetToolVer :: Maybe ToolVersion -> SetToolVersion
toSetToolVer (Just (ToolVersion v')) = SetToolVersion v'
toSetToolVer (Just (ToolTag t')) = SetToolTag t'
toSetToolVer Nothing = SetRecommended


data InstallCommand = InstallGHC InstallOptions
                    | InstallCabal InstallOptions
                    | InstallHLS InstallOptions
                    | InstallStack InstallOptions

data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instPlatform :: Maybe PlatformRequest
  , instBindist  :: Maybe URI
  , instSet      :: Bool
  }

data SetCommand = SetGHC SetOptions
                | SetCabal SetOptions
                | SetHLS SetOptions
                | SetStack SetOptions

-- a superset of ToolVersion
data SetToolVersion = SetToolVersion GHCTargetVersion
                    | SetToolTag Tag
                    | SetRecommended
                    | SetNext

data SetOptions = SetOptions
  { sToolVer :: SetToolVersion
  }

data ListOptions = ListOptions
  { loTool     :: Maybe Tool
  , lCriteria  :: Maybe ListCriteria
  , lRawFormat :: Bool
  }

data RmCommand = RmGHC RmOptions
               | RmCabal Version
               | RmHLS Version
               | RmStack Version

data RmOptions = RmOptions
  { ghcVer :: GHCTargetVersion
  }


data CompileCommand = CompileGHC GHCCompileOptions

data GHCCompileOptions = GHCCompileOptions
  { targetGhc    :: Either Version GitBranch
  , bootstrapGhc :: Either Version FilePath
  , jobs         :: Maybe Int
  , buildConfig  :: Maybe FilePath
  , patchDir     :: Maybe FilePath
  , crossTarget  :: Maybe Text
  , addConfArgs  :: [Text]
  , setCompile   :: Bool
  }

data UpgradeOpts = UpgradeInplace
                 | UpgradeAt FilePath
                 | UpgradeGHCupDir
                 deriving Show

data ChangeLogOptions = ChangeLogOptions
  { clOpen    :: Bool
  , clTool    :: Maybe Tool
  , clToolVer :: Maybe ToolVersion
  }


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
    ( flag' True (enmod <> long longopt <> if defv then mempty else short shortopt)
    <|> flag' False (dismod <> long nolongopt <> if defv then short shortopt else mempty)
    )
  where
    nolongopt = "no-" ++ longopt


opts :: Parser Options
opts =
  Options
    <$> invertableSwitch "verbose" 'v' False (help "Enable verbosity (default: disabled)")
    <*> invertableSwitch "cache" 'c' False (help "Cache downloads in ~/.ghcup/cache (default: disabled)")
    <*> (optional
          (option
            (eitherReader parseUri)
            (  short 's'
            <> long "url-source"
            <> metavar "URL"
            <> help "Alternative ghcup download info url"
            <> internal
            )
          )
        )
    <*> (fmap . fmap) not (invertableSwitch "verify" 'n' True (help "Disable tarball checksum verification (default: enabled)"))
    <*> optional (option
          (eitherReader keepOnParser)
          (  long "keep"
          <> metavar "<always|errors|never>"
          <> help
               "Keep build directories? (default: errors)"
          <> hidden
          ))
    <*> optional (option
          (eitherReader downloaderParser)
          (  long "downloader"
#if defined(INTERNAL_DOWNLOADER)
          <> metavar "<internal|curl|wget>"
          <> help
          "Downloader to use (default: internal)"
#else
          <> metavar "<curl|wget>"
          <> help
          "Downloader to use (default: curl)"
#endif
          <> hidden
          ))
    <*> com
 where
  parseUri s' =
    first show $ parseURI strictURIParserOptions (UTF8.fromString s')


com :: Parser Command
com =
  subparser
#if defined(BRICK)
      (  command
          "tui"
          (   (\_ -> Interactive)
          <$> (info
                helper
                (  progDesc "Start the interactive GHCup UI"
                )
              )
          )
      <>  command
#else
      (  command
#endif
          "install"
          (   Install
          <$> info
                (installParser <**> helper)
                (  progDesc "Install or update GHC/cabal"
                <> footerDoc (Just $ text installToolFooter)
                )
          )
      <> command
           "set"
           (info
             (Set <$> setParser <**> helper)
             (  progDesc "Set currently active GHC/cabal version"
             <> footerDoc (Just $ text setFooter)
             )
           )
      <> command
           "rm"
           (info
             (Rm <$> rmParser <**> helper)
             (  progDesc "Remove a GHC/cabal version"
             <> footerDoc (Just $ text rmFooter)
             )
           )

      <> command
           "list"
           (info (List <$> listOpts <**> helper)
                 (progDesc "Show available GHCs and other tools")
           )
      <> command
           "upgrade"
           (info
             (    (Upgrade <$> upgradeOptsP <*> switch
                    (short 'f' <> long "force" <> help "Force update")
                  )
             <**> helper
             )
             (progDesc "Upgrade ghcup")
           )
      <> command
           "compile"
           (   Compile
           <$> info (compileP <**> helper)
                    (progDesc "Compile a tool from source")
           )
      <> commandGroup "Main commands:"
      )
    <|> subparser
          (  command
              "debug-info"
              ((\_ -> DInfo) <$> info helper (progDesc "Show debug info"))
          <> command
               "tool-requirements"
               (   (\_ -> ToolRequirements)
               <$> info helper
                        (progDesc "Show the requirements for ghc/cabal")
               )
          <> command
               "changelog"
               (info
                  (fmap ChangeLog changelogP <**> helper)
                  (  progDesc "Find/show changelog"
                  <> footerDoc (Just $ text changeLogFooter)
                  )
               )
          <> commandGroup "Other commands:"
          <> hidden
          )
    <|> subparser
          (  command
              "install-cabal"
              (info
                 ((InstallCabalLegacy <$> installOpts (Just Cabal)) <**> helper)
                 (  progDesc "Install or update cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
              )
          <> internal
          )
 where
  installToolFooter :: String
  installToolFooter = [s|Discussion:
  Installs GHC or cabal. When no command is given, installs GHC
  with the specified version/tag.
  It is recommended to always specify a subcommand (ghc/cabal/hls).|]

  setFooter :: String
  setFooter = [s|Discussion:
  Sets the currently active GHC or cabal version. When no command is given,
  defaults to setting GHC with the specified version/tag (if no tag
  is given, sets GHC to 'recommended' version).
  It is recommended to always specify a subcommand (ghc/cabal/hls).|]

  rmFooter :: String
  rmFooter = [s|Discussion:
  Remove the given GHC or cabal version. When no command is given,
  defaults to removing GHC with the specified version.
  It is recommended to always specify a subcommand (ghc/cabal/hls).|]

  changeLogFooter :: String
  changeLogFooter = [s|Discussion:
  By default returns the URI of the ChangeLog of the latest GHC release.
  Pass '-o' to automatically open via xdg-open.|]


installCabalFooter :: String
installCabalFooter = [s|Discussion:
  Installs the specified cabal-install version (or a recommended default one)
  into "~/.ghcup/bin", so it can be overwritten by later
  "cabal install cabal-install", which installs into "~/.cabal/bin" by
  default. Make sure to set up your PATH appropriately, so the cabal
  installation takes precedence.|]


installParser :: Parser (Either InstallCommand InstallOptions)
installParser =
  (Left <$> subparser
      (  command
          "ghc"
          (   InstallGHC
          <$> info
                (installOpts (Just GHC) <**> helper)
                (  progDesc "Install GHC"
                <> footerDoc (Just $ text installGHCFooter)
                )
          )
      <> command
           "cabal"
           (   InstallCabal
           <$> info
                 (installOpts (Just Cabal) <**> helper)
                 (  progDesc "Install Cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
           )
      <> command
           "hls"
           (   InstallHLS
           <$> info
                 (installOpts (Just HLS) <**> helper)
                 (  progDesc "Install haskell-languge-server"
                 <> footerDoc (Just $ text installHLSFooter)
                 )
           )
      <> command
           "stack"
           (   InstallStack
           <$> info
                 (installOpts (Just Stack) <**> helper)
                 (  progDesc "Install stack"
                 <> footerDoc (Just $ text installStackFooter)
                 )
           )
      )
    )
    <|> (Right <$> installOpts Nothing)
 where
  installHLSFooter :: String
  installHLSFooter = [s|Discussion:
  Installs haskell-language-server binaries and wrapper
  into "~/.ghcup/bin"

Examples:
  # install recommended HLS
  ghcup install hls|]

  installStackFooter :: String
  installStackFooter = [s|Discussion:
  Installs stack binaries into "~/.ghcup/bin"

Examples:
  # install recommended Stack
  ghcup install stack|]

  installGHCFooter :: String
  installGHCFooter = [s|Discussion:
  Installs the specified GHC version (or a recommended default one) into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".

Examples:
  # install recommended GHC
  ghcup install ghc

  # install latest GHC
  ghcup install ghc latest

  # install GHC 8.10.2
  ghcup install ghc 8.10.2

  # install GHC head fedora bindist
  ghcup install ghc -u https://gitlab.haskell.org/api/v4/projects/1/jobs/artifacts/master/raw/ghc-x86_64-fedora27-linux.tar.xz?job=validate-x86_64-linux-fedora27 head|]


installOpts :: Maybe Tool -> Parser InstallOptions
installOpts tool =
  (\p (u, v) b -> InstallOptions v p u b)
    <$> optional
          (option
            (eitherReader platformParser)
            (  short 'p'
            <> long "platform"
            <> metavar "PLATFORM"
            <> help
                 "Override for platform (triple matching ghc tarball names), e.g. x86_64-fedora27-linux"
            )
          )
    <*> (   (   (,)
            <$> optional
                  (option
                    (eitherReader bindistParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                    )
                  )
            <*> (Just <$> toolVersionArgument Nothing tool)
            )
        <|> pure (Nothing, Nothing)
        )
    <*> flag
          False
          True
          (long "set" <> help
            "Set as active version after install"
          )


setParser :: Parser (Either SetCommand SetOptions)
setParser =
  (Left <$> subparser
      (  command
          "ghc"
          (   SetGHC
          <$> info
                (setOpts (Just GHC) <**> helper)
                (  progDesc "Set GHC version"
                <> footerDoc (Just $ text setGHCFooter)
                )
          )
      <> command
           "cabal"
           (   SetCabal
           <$> info
                 (setOpts (Just Cabal) <**> helper)
                 (  progDesc "Set Cabal version"
                 <> footerDoc (Just $ text setCabalFooter)
                 )
           )
      <> command
           "hls"
           (   SetHLS
           <$> info
                 (setOpts (Just HLS) <**> helper)
                 (  progDesc "Set haskell-language-server version"
                 <> footerDoc (Just $ text setHLSFooter)
                 )
           )
      <> command
           "stack"
           (   SetStack
           <$> info
                 (setOpts (Just Stack) <**> helper)
                 (  progDesc "Set stack version"
                 <> footerDoc (Just $ text setStackFooter)
                 )
           )
      )
    )
    <|> (Right <$> setOpts Nothing)
 where
  setGHCFooter :: String
  setGHCFooter = [s|Discussion:
    Sets the the current GHC version by creating non-versioned
    symlinks for all ghc binaries of the specified version in
    "~/.ghcup/bin/<binary>".|]

  setCabalFooter :: String
  setCabalFooter = [s|Discussion:
    Sets the the current Cabal version.|]

  setStackFooter :: String
  setStackFooter = [s|Discussion:
    Sets the the current Stack version.|]

  setHLSFooter :: String
  setHLSFooter = [s|Discussion:
    Sets the the current haskell-language-server version.|]


setOpts :: Maybe Tool -> Parser SetOptions
setOpts tool = SetOptions <$>
    (fromMaybe SetRecommended <$>
      optional (setVersionArgument (Just ListInstalled) tool))

listOpts :: Parser ListOptions
listOpts =
  ListOptions
    <$> optional
          (option
            (eitherReader toolParser)
            (short 't' <> long "tool" <> metavar "<ghc|cabal>" <> help
              "Tool to list versions for. Default is all"
            )
          )
    <*> optional
          (option
            (eitherReader criteriaParser)
            (  short 'c'
            <> long "show-criteria"
            <> metavar "<installed|set>"
            <> help "Show only installed or set tool versions"
            )
          )
    <*> switch
          (short 'r' <> long "raw-format" <> help "More machine-parsable format"
          )


rmParser :: Parser (Either RmCommand RmOptions)
rmParser =
  (Left <$> subparser
      (  command
          "ghc"
          (RmGHC <$> info (rmOpts (Just GHC) <**> helper) (progDesc "Remove GHC version"))
      <> command
           "cabal"
           (   RmCabal
           <$> info (versionParser' (Just ListInstalled) (Just Cabal) <**> helper)
                    (progDesc "Remove Cabal version")
           )
      <> command
           "hls"
           (   RmHLS
           <$> info (versionParser' (Just ListInstalled) (Just HLS) <**> helper)
                    (progDesc "Remove haskell-language-server version")
           )
      <> command
           "stack"
           (   RmStack
           <$> info (versionParser' (Just ListInstalled) (Just Stack) <**> helper)
                    (progDesc "Remove stack version")
           )
      )
    )
    <|> (Right <$> rmOpts Nothing)



rmOpts :: Maybe Tool -> Parser RmOptions
rmOpts tool = RmOptions <$> versionArgument (Just ListInstalled) tool


changelogP :: Parser ChangeLogOptions
changelogP =
  (\x y -> ChangeLogOptions x y)
    <$> switch (short 'o' <> long "open" <> help "xdg-open the changelog url")
    <*> optional
          (option
            (eitherReader
              (\s' -> case fmap toLower s' of
                "ghc"   -> Right GHC
                "cabal" -> Right Cabal
                "ghcup" -> Right GHCup
                "stack" -> Right Stack
                e       -> Left e
              )
            )
            (short 't' <> long "tool" <> metavar "<ghc|cabal|ghcup>" <> help
              "Open changelog for given tool (default: ghc)"
            )
          )
    <*> optional (toolVersionArgument Nothing Nothing)

compileP :: Parser CompileCommand
compileP = subparser
  (  command
      "ghc"
      (   CompileGHC
      <$> info
            (ghcCompileOpts <**> helper)
            (  progDesc "Compile GHC from source"
            <> footerDoc (Just $ text compileFooter)
            )
      )
  )
 where
  compileFooter = [s|Discussion:
  Compiles and installs the specified GHC version into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".

  This also allows building a cross-compiler. Consult the documentation
  first: <https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling#configuring-the-build>

ENV variables:
  Various toolchain variables will be passed onto the ghc build system,
  such as: CC, LD, OBJDUMP, NM, AR, RANLIB.

Examples:
  # compile from known version
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2
  # compile from git commit/reference
  ghcup compile ghc -j 4 -g master -b 8.2.2
  # specify path to bootstrap ghc
  ghcup compile ghc -j 4 -v 8.4.2 -b /usr/bin/ghc-8.2.2
  # build cross compiler
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2 -x armv7-unknown-linux-gnueabihf --config $(pwd)/build.mk -- --enable-unregisterised|]


ghcCompileOpts :: Parser GHCCompileOptions
ghcCompileOpts =
  GHCCompileOptions
    <$> ((Left <$> option
          (eitherReader
            (first (const "Not a valid version") . version . T.pack)
          )
          (short 'v' <> long "version" <> metavar "VERSION" <> help
            "The tool version to compile"
          )
          ) <|>
          (Right <$> (GitBranch <$> option
          str
          (short 'g' <> long "git-ref" <> metavar "GIT_REFERENCE" <> help
            "The git commit/branch/ref to build from"
          ) <*>
          optional (option str (short 'r' <> long "repository" <> metavar "GIT_REPOSITORY" <> help "The git repository to build from (defaults to GHC upstream)"))
          )))
    <*> option
          (eitherReader
            (\x ->
              (bimap (const "Not a valid version") Left . version . T.pack $ x) <|> (if isPathSeparator (head x) then pure $ Right x else Left "Not an absolute Path")
            )
          )
          (  short 'b'
          <> long "bootstrap-ghc"
          <> metavar "BOOTSTRAP_GHC"
          <> help
               "The GHC version (or full path) to bootstrap with (must be installed)"
          )
    <*> optional
          (option
            (eitherReader (readEither @Int))
            (short 'j' <> long "jobs" <> metavar "JOBS" <> help
              "How many jobs to use for make"
            )
          )
    <*> optional
          (option
            str
            (short 'c' <> long "config" <> metavar "CONFIG" <> help
              "Absolute path to build config file"
            )
          )
    <*> optional
          (option
            str
            (short 'p' <> long "patchdir" <> metavar "PATCH_DIR" <> help
              "Absolute path to patch directory (applied in order, uses -p1)"
            )
          )
    <*> optional
          (option
            str
            (short 'x' <> long "cross-target" <> metavar "CROSS_TARGET" <> help
              "Build cross-compiler for this platform"
            )
          )
    <*> many (argument str (metavar "CONFIGURE_ARGS" <> help "Additional arguments to configure, prefix with '-- ' (longopts)"))
    <*> flag
          False
          True
          (long "set" <> help
            "Set as active version after install"
          )


toolVersionParser :: Parser ToolVersion
toolVersionParser = verP' <|> toolP
 where
  verP' = ToolVersion <$> versionParser
  toolP =
    ToolTag
      <$> option
            (eitherReader tagEither)
            (short 't' <> long "tag" <> metavar "TAG" <> help "The target tag")

-- | same as toolVersionParser, except as an argument.
toolVersionArgument :: Maybe ListCriteria -> Maybe Tool -> Parser ToolVersion
toolVersionArgument criteria tool =
  argument (eitherReader toolVersionEither)
    (metavar "VERSION|TAG"
    <> completer (tagCompleter (fromMaybe GHC tool) [])
    <> foldMap (completer . versionCompleter criteria) tool)


setVersionArgument :: Maybe ListCriteria -> Maybe Tool -> Parser SetToolVersion
setVersionArgument criteria tool =
  argument (eitherReader setEither)
    (metavar "VERSION|TAG|next"
    <> completer (tagCompleter (fromMaybe GHC tool) ["next"])
    <> foldMap (completer . versionCompleter criteria) tool)
 where
  setEither s' =
        parseSet s'
    <|> second SetToolTag (tagEither s')
    <|> second SetToolVersion (tVersionEither s')
  parseSet s' = case fmap toLower s' of
                  "next" -> Right SetNext
                  other  -> Left [i|Unknown tag/version #{other}|]


versionArgument :: Maybe ListCriteria -> Maybe Tool -> Parser GHCTargetVersion
versionArgument criteria tool = argument (eitherReader tVersionEither) (metavar "VERSION" <> foldMap (completer . versionCompleter criteria) tool)


tagCompleter :: Tool -> [String] -> Completer
tagCompleter tool add = listIOCompleter $ do
  let loggerConfig = LoggerConfig
        { lcPrintDebug = False
        , colorOutter  = mempty
        , rawOutter    = mempty
        }

      runLogger = myLoggerT loggerConfig

  dirs <- getDirs
  let simpleSettings = Settings False False Never Curl False GHCupURL
      simpleAppState = AppState simpleSettings dirs defaultKeyBindings
      runEnv = runLogger . flip runReaderT simpleAppState

  mGhcUpInfo <- runEnv . runE $ readFromCache

  case mGhcUpInfo of
    VRight dls -> do
      let allTags = filter (\t -> t /= Old)
            $ join
            $ M.elems
            $ availableToolVersions (_ghcupDownloads dls) tool
      pure $ nub $ (add ++) $ fmap tagToString allTags
    VLeft _ -> pure  (nub $ ["recommended", "latest"] ++ add)


versionCompleter :: Maybe ListCriteria -> Tool -> Completer
versionCompleter criteria tool = listIOCompleter $ do
  let loggerConfig = LoggerConfig
        { lcPrintDebug = False
        , colorOutter  = mempty
        , rawOutter    = mempty
        }

      runLogger = myLoggerT loggerConfig

  mpFreq <- runLogger . runE $ platformRequest

  forFold mpFreq $ \pfreq -> do
    dirs <- getDirs
    let simpleSettings = Settings False False Never Curl False GHCupURL
        simpleAppState = AppState simpleSettings dirs defaultKeyBindings
        runEnv = runLogger . flip runReaderT simpleAppState

    mGhcUpInfo <- runEnv . runE $ readFromCache

    forFold mGhcUpInfo $ \(GHCupInfo _ dls) -> do
      installedVersions <- runEnv $ listVersions dls (Just tool) criteria pfreq
      return $ T.unpack . prettyVer . lVer <$> installedVersions


versionParser :: Parser GHCTargetVersion
versionParser = option
  (eitherReader tVersionEither)
  (short 'v' <> long "version" <> metavar "VERSION" <> help "The target version"
  )

versionParser' :: Maybe ListCriteria -> Maybe Tool -> Parser Version
versionParser' criteria tool = argument
  (eitherReader (first show . version . T.pack))
  (metavar "VERSION"  <> foldMap (completer . versionCompleter criteria) tool)


tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended" -> Right Recommended
  "latest"      -> Right Latest
  ('b':'a':'s':'e':'-':ver') -> case pvp (T.pack ver') of
                                  Right x -> Right (Base x)
                                  Left  _ -> Left [i|Invalid PVP version for base #{ver'}|]
  other         -> Left [i|Unknown tag #{other}|]


tVersionEither :: String -> Either String GHCTargetVersion
tVersionEither =
  first (const "Not a valid version") . MP.parse ghcTargetVerP "" . T.pack


toolVersionEither :: String -> Either String ToolVersion
toolVersionEither s' =
  second ToolTag (tagEither s') <|> second ToolVersion (tVersionEither s')


toolParser :: String -> Either String Tool
toolParser s' | t == T.pack "ghc"   = Right GHC
              | t == T.pack "cabal" = Right Cabal
              | otherwise           = Left ("Unknown tool: " <> s')
  where t = T.toLower (T.pack s')


criteriaParser :: String -> Either String ListCriteria
criteriaParser s' | t == T.pack "installed" = Right ListInstalled
                  | t == T.pack "set"       = Right ListSet
                  | otherwise               = Left ("Unknown criteria: " <> s')
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


platformParser :: String -> Either String PlatformRequest
platformParser s' = case MP.parse (platformP <* MP.eof) "" (T.pack s') of
  Right r -> pure r
  Left  e -> Left $ errorBundlePretty e
 where
  archP :: MP.Parsec Void Text Architecture
  archP = MP.try (MP.chunk "x86_64" $> A_64) <|> (MP.chunk "i386" $> A_32)
  platformP :: MP.Parsec Void Text PlatformRequest
  platformP = choice'
    [ (\a mv -> PlatformRequest a FreeBSD mv)
    <$> (archP <* MP.chunk "-")
    <*> (  MP.chunk "portbld"
        *> (   MP.try (Just <$> verP (MP.chunk "-freebsd" <* MP.eof))
           <|> pure Nothing
           )
        <* MP.chunk "-freebsd"
        )
    , (\a mv -> PlatformRequest a Darwin mv)
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


toSettings :: Options -> IO AppState
toSettings options = do
  dirs <- getDirs
  userConf <- runE @'[ JSONError ] ghcupConfigFile >>= \case
    VRight r -> pure r
    VLeft (V (JSONDecodeError e)) -> do
      B.hPut stderr ("Error decoding config file: " <> (E.encodeUtf8 . T.pack . show $ e))
      pure defaultUserSettings
    _ -> do
      die "Unexpected error!"
  pure $ mergeConf options dirs userConf
 where
   mergeConf :: Options -> Dirs -> UserSettings -> AppState
   mergeConf Options{..} dirs UserSettings{..} =
     let cache       = fromMaybe (fromMaybe False uCache) optCache
         noVerify    = fromMaybe (fromMaybe False uNoVerify) optNoVerify
         verbose     = fromMaybe (fromMaybe False uVerbose) optVerbose
         keepDirs    = fromMaybe (fromMaybe Errors uKeepDirs) optKeepDirs
         downloader  = fromMaybe (fromMaybe defaultDownloader uDownloader) optsDownloader
         keyBindings = maybe defaultKeyBindings mergeKeys uKeyBindings
         urlSource   = maybe (fromMaybe GHCupURL uUrlSource) OwnSource optUrlSource
     in AppState (Settings {..}) dirs keyBindings
#if defined(INTERNAL_DOWNLOADER)
   defaultDownloader = Internal
#else
   defaultDownloader = Curl
#endif
   mergeKeys :: UserKeyBindings -> KeyBindings
   mergeKeys UserKeyBindings {..} =
     let KeyBindings {..} = defaultKeyBindings
     in KeyBindings {
           bUp = fromMaybe bUp kUp
         , bDown = fromMaybe bDown kDown
         , bQuit = fromMaybe bQuit kQuit
         , bInstall = fromMaybe bInstall kInstall
         , bUninstall = fromMaybe bUninstall kUninstall
         , bSet = fromMaybe bSet kSet
         , bChangelog = fromMaybe bChangelog kChangelog
         , bShowAllVersions = fromMaybe bShowAllVersions kShowAll
         , bShowAllTools = fromMaybe bShowAllTools kShowAllTools
         }


upgradeOptsP :: Parser UpgradeOpts
upgradeOptsP =
  flag'
      UpgradeInplace
      (short 'i' <> long "inplace" <> help
        "Upgrade ghcup in-place (wherever it's at)"
      )
    <|> (   UpgradeAt
        <$> option
              str
              (short 't' <> long "target" <> metavar "TARGET_DIR" <> help
                "Absolute filepath to write ghcup into"
              )
        )
    <|> pure UpgradeGHCupDir



describe_result :: String
describe_result = $( LitE . StringL <$>
                     runIO (do
                             CapturedProcess{..} <- executeOut "git" ["describe"] Nothing
                             case _exitCode of
                               ExitSuccess   -> pure . T.unpack . decUTF8Safe' $ _stdOut
                               ExitFailure _ -> pure numericVer
                     )
                   )


main :: IO ()
main = do
  let versionHelp = infoOption
        ( ("The GHCup Haskell installer, version " <>)
          (head . lines $ describe_result)
        )
        (long "version" <> help "Show version" <> hidden)
  let numericVersionHelp = infoOption
        numericVer
        (  long "numeric-version"
        <> help "Show the numeric version (for use in scripts)"
        <> hidden
        )
  let listCommands = infoOption
        "install set rm install-cabal list upgrade compile debug-info tool-requirements changelog"
        (  long "list-commands"
        <> help "List available commands for shell completion"
        <> internal
        )

  let main_footer = [s|Discussion:
  ghcup installs the Glasgow Haskell Compiler from the official
  release channels, enabling you to easily switch between different
  versions. It maintains a self-contained ~/.ghcup directory.

ENV variables:
  * TMPDIR: where ghcup does the work (unpacking, building, ...)
  * GHCUP_INSTALL_BASE_PREFIX: the base of ghcup (default: $HOME)
  * GHCUP_USE_XDG_DIRS: set to anything to use XDG style directories

Report bugs at <https://gitlab.haskell.org/haskell/ghcup-hs/issues>|]

  customExecParser
      (prefs showHelpOnError)
      (info (opts <**> helper <**> versionHelp <**> numericVersionHelp <**> listCommands)
            (footerDoc (Just $ text main_footer))
      )
    >>= \opt@Options {..} -> do
          appstate@AppState{dirs = Dirs{..}, ..} <- toSettings opt

          -- create ~/.ghcup dir
          createDirRecursive' baseDir

          -- logger interpreter
          logfile <- flip runReaderT appstate $ initGHCupFileLogging
          let loggerConfig = LoggerConfig
                { lcPrintDebug = verbose settings
                , colorOutter  = B.hPut stderr
                , rawOutter    = B.appendFile logfile
                }
          let runLogger = myLoggerT loggerConfig


          -------------------------
          -- Effect interpreters --
          -------------------------

          let runInstTool' appstate' =
                runLogger
                  . flip runReaderT appstate'
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , UnknownArchive
#if !defined(TAR)
                      , ArchiveResult
#endif
                      , FileDoesNotExistError
                      , CopyError
                      , NoDownload
                      , NotInstalled
                      , BuildFailed
                      , TagNotFound
                      , DigestError
                      , DownloadFailed
                      , TarDirDoesNotExist
                      , NextVerNotFound
                      , NoToolVersionSet
                      ]

          let runInstTool = runInstTool' appstate

          let
            runSetGHC =
              runLogger
                . flip runReaderT appstate
                . runE
                  @'[ FileDoesNotExistError
                    , NotInstalled
                    , TagNotFound
                    , NextVerNotFound
                    , NoToolVersionSet
                    ]

          let
            runSetCabal =
              runLogger
                . flip runReaderT appstate
                . runE
                  @'[ NotInstalled
                    , TagNotFound
                    , NextVerNotFound
                    , NoToolVersionSet
                    ]

          let
            runSetHLS =
              runLogger
                . flip runReaderT appstate
                . runE
                  @'[ NotInstalled
                    , TagNotFound
                    , NextVerNotFound
                    , NoToolVersionSet
                    ]

          let runListGHC = runLogger . flip runReaderT appstate

          let runRm =
                runLogger . flip runReaderT appstate . runE @'[NotInstalled]

          let runDebugInfo =
                runLogger
                  . flip runReaderT appstate
                  . runE
                    @'[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]

          let runCompileGHC =
                runLogger
                  . flip runReaderT appstate
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , BuildFailed
                      , DigestError
                      , DownloadFailed
                      , GHCupSetError
                      , NoDownload
                      , NotFoundInPATH
                      , PatchFailed
                      , UnknownArchive
                      , TarDirDoesNotExist
                      , NotInstalled
#if !defined(TAR)
                      , ArchiveResult
#endif
                      ]

          let runUpgrade =
                runLogger
                  . flip runReaderT appstate
                  . runResourceT
                  . runE
                    @'[ DigestError
                      , NoDownload
                      , NoUpdate
                      , FileDoesNotExistError
                      , CopyError
                      , DownloadFailed
                      ]


          ----------------------------------------
          -- Getting download and platform info --
          ----------------------------------------

          pfreq <- (
            runLogger . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] . liftE $ platformRequest
            ) >>= \case
                    VRight r -> pure r
                    VLeft e -> do
                      runLogger
                        ($(logError) $ T.pack $ prettyShow e)
                      exitWith (ExitFailure 2)


          (GHCupInfo treq dls) <-
            ( runLogger
              . flip runReaderT appstate
              . runE @'[JSONError , DownloadFailed, FileDoesNotExistError]
              $ liftE
              $ getDownloadsF (urlSource settings)
              )
              >>= \case
                    VRight r -> pure r
                    VLeft  e -> do
                      runLogger
                        ($(logError) $ T.pack $ prettyShow e)
                      exitWith (ExitFailure 2)

          case optCommand of
            Upgrade _ _ -> pure ()
            _ -> runLogger $ flip runReaderT appstate $ checkForUpdates dls pfreq



          -----------------------
          -- Command functions --
          -----------------------

          let installGHC InstallOptions{..} =
                  (case instBindist of
                     Nothing -> runInstTool $ do
                       (v, vi) <- liftE $ fromVersion dls instVer GHC
                       liftE $ installGHCBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                       when instSet $ void $ liftE $ setGHC v SetGHCOnly
                       pure vi
                     Just uri -> runInstTool' appstate{ settings = settings {noVerify = True}} $ do
                       (v, vi) <- liftE $ fromVersion dls instVer GHC
                       liftE $ installGHCBindist
                         (DownloadInfo uri (Just $ RegexDir "ghc-.*") "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                       when instSet $ void $ liftE $ setGHC v SetGHCOnly
                       pure vi
                    )
                    >>= \case
                          VRight vi -> do
                            runLogger $ $(logInfo) "GHC installation successful"
                            forM_ (_viPostInstall =<< vi) $ \msg ->
                              runLogger $ $(logInfo) msg
                            pure ExitSuccess
                          VLeft (V (AlreadyInstalled _ v)) -> do
                            runLogger $ $(logWarn)
                              [i|GHC ver #{prettyVer v} already installed; if you really want to reinstall it, you may want to run 'ghcup rm ghc #{prettyVer v}' first|]
                            pure ExitSuccess
                          VLeft err@(V (BuildFailed tmpdir _)) -> do
                            case keepDirs settings of
                              Never -> runLogger ($(logError) $ T.pack $ prettyShow err)
                              _ -> runLogger ($(logError) [i|#{prettyShow err}
    Check the logs at #{logsDir} and the build directory #{tmpdir} for more clues.
    Make sure to clean up #{tmpdir} afterwards.|])
                            pure $ ExitFailure 3
                          VLeft e -> do
                            runLogger $ do
                              $(logError) $ T.pack $ prettyShow e
                              $(logError) [i|Also check the logs in #{logsDir}|]
                            pure $ ExitFailure 3


          let installCabal InstallOptions{..} =
                (case instBindist of
                   Nothing -> runInstTool $ do
                     (v, vi) <- liftE $ fromVersion dls instVer Cabal
                     liftE $ installCabalBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                     pure vi
                   Just uri -> runInstTool' appstate{ settings = settings { noVerify = True}} $ do
                     (v, vi) <- liftE $ fromVersion dls instVer Cabal
                     liftE $ installCabalBindist
                         (DownloadInfo uri Nothing "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                     pure vi
                  )
                  >>= \case
                        VRight vi -> do
                          runLogger $ $(logInfo) "Cabal installation successful"
                          forM_ (_viPostInstall =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft (V (AlreadyInstalled _ v)) -> do
                          runLogger $ $(logWarn)
                            [i|Cabal ver #{prettyVer v} already installed; if you really want to reinstall it, you may want to run 'ghcup rm cabal #{prettyVer v}' first|]
                          pure ExitSuccess
                        VLeft e -> do
                          runLogger $ do
                            $(logError) $ T.pack $ prettyShow e
                            $(logError) [i|Also check the logs in #{logsDir}|]
                          pure $ ExitFailure 4

          let installHLS InstallOptions{..} =
                (case instBindist of
                   Nothing -> runInstTool $ do
                     (v, vi) <- liftE $ fromVersion dls instVer HLS
                     liftE $ installHLSBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                     pure vi
                   Just uri -> runInstTool' appstate{ settings = settings { noVerify = True}} $ do
                     (v, vi) <- liftE $ fromVersion dls instVer HLS
                     liftE $ installHLSBindist
                         (DownloadInfo uri Nothing "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                     pure vi
                  )
                  >>= \case
                        VRight vi -> do
                          runLogger $ $(logInfo) "HLS installation successful"
                          forM_ (_viPostInstall =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft (V (AlreadyInstalled _ v)) -> do
                          runLogger $ $(logWarn)
                            [i|HLS ver #{prettyVer v} already installed; if you really want to reinstall it, you may want to run 'ghcup rm hls #{prettyVer v}' first|]
                          pure ExitSuccess
                        VLeft e -> do
                          runLogger $ do
                            $(logError) $ T.pack $ prettyShow e
                            $(logError) [i|Also check the logs in #{logsDir}|]
                          pure $ ExitFailure 4

          let installStack InstallOptions{..} =
                (case instBindist of
                   Nothing -> runInstTool $ do
                     (v, vi) <- liftE $ fromVersion dls instVer Stack
                     liftE $ installStackBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                     pure vi
                   Just uri -> runInstTool' appstate{ settings = settings { noVerify = True}} $ do
                     (v, vi) <- liftE $ fromVersion dls instVer Stack
                     liftE $ installStackBindist
                         (DownloadInfo uri Nothing "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                     pure vi
                  )
                  >>= \case
                        VRight vi -> do
                          runLogger $ $(logInfo) "Stack installation successful"
                          forM_ (_viPostInstall =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft (V (AlreadyInstalled _ v)) -> do
                          runLogger $ $(logWarn)
                            [i|Stack ver #{prettyVer v} already installed; if you really want to reinstall it, you may want to run 'ghcup rm stack #{prettyVer v}' first|]
                          pure ExitSuccess
                        VLeft e -> do
                          runLogger $ do
                            $(logError) $ T.pack $ prettyShow e
                            $(logError) [i|Also check the logs in #{logsDir}|]
                          pure $ ExitFailure 4


          let setGHC' SetOptions{..} =
                runSetGHC (do
                    v <- liftE $ fst <$> fromVersion' dls sToolVer GHC
                    liftE $ setGHC v SetGHCOnly
                  )
                  >>= \case
                        VRight GHCTargetVersion{..} -> do
                          runLogger
                            $ $(logInfo)
                                [i|GHC #{prettyVer _tvVersion} successfully set as default version#{maybe "" (" for cross target " <>) _tvTarget}|]
                          pure ExitSuccess
                        VLeft e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 5

          let setCabal' SetOptions{..} =
                runSetCabal (do
                    v <- liftE $ fst <$> fromVersion' dls sToolVer Cabal
                    liftE $ setCabal (_tvVersion v)
                    pure v
                  )
                  >>= \case
                        VRight GHCTargetVersion{..} -> do
                          runLogger
                            $ $(logInfo)
                                [i|Cabal #{prettyVer _tvVersion} successfully set as default version|]
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 14

          let setHLS' SetOptions{..} =
                runSetHLS (do
                    v <- liftE $ fst <$> fromVersion' dls sToolVer HLS
                    liftE $ setHLS (_tvVersion v)
                    pure v
                  )
                  >>= \case
                        VRight GHCTargetVersion{..} -> do
                          runLogger
                            $ $(logInfo)
                                [i|HLS #{prettyVer _tvVersion} successfully set as default version|]
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 14

          let setStack' SetOptions{..} =
                runSetCabal (do
                    v <- liftE $ fst <$> fromVersion' dls sToolVer Stack
                    liftE $ setStack (_tvVersion v)
                    pure v
                  )
                  >>= \case
                        VRight GHCTargetVersion{..} -> do
                          runLogger
                            $ $(logInfo)
                                [i|Stack #{prettyVer _tvVersion} successfully set as default version|]
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 14

          let rmGHC' RmOptions{..} =
                runRm (do
                    liftE $
                      rmGHCVer ghcVer
                    pure (getVersionInfo (_tvVersion ghcVer) GHC dls)
                  )
                  >>= \case
                        VRight vi -> do
                          forM_ (_viPostRemove =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 7

          let rmCabal' tv =
                runRm (do
                    liftE $
                      rmCabalVer tv
                    pure (getVersionInfo tv Cabal dls)
                  )
                  >>= \case
                        VRight vi -> do
                          forM_ (_viPostRemove =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 15

          let rmHLS' tv =
                runRm (do
                    liftE $
                      rmHLSVer tv
                    pure (getVersionInfo tv HLS dls)
                  )
                  >>= \case
                        VRight vi -> do
                          forM_ (_viPostRemove =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 15

          let rmStack' tv =
                runRm (do
                    liftE $
                      rmStackVer tv
                    pure (getVersionInfo tv Stack dls)
                  )
                  >>= \case
                        VRight vi -> do
                          forM_ (_viPostRemove =<< vi) $ \msg ->
                            runLogger $ $(logInfo) msg
                          pure ExitSuccess
                        VLeft  e -> do
                          runLogger $ $(logError) $ T.pack $ prettyShow e
                          pure $ ExitFailure 15

          res <- case optCommand of
#if defined(BRICK)
            Interactive -> liftIO $ brickMain appstate loggerConfig dls pfreq >> pure ExitSuccess
#endif
            Install (Right iopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for installing GHC. Use 'ghcup install ghc' instead.|])
              installGHC iopts
            Install (Left (InstallGHC iopts)) -> installGHC iopts
            Install (Left (InstallCabal iopts)) -> installCabal iopts
            Install (Left (InstallHLS iopts)) -> installHLS iopts
            Install (Left (InstallStack iopts)) -> installStack iopts
            InstallCabalLegacy iopts -> do
              runLogger ($(logWarn) [i|This is an old-style command for installing cabal. Use 'ghcup install cabal' instead.|])
              installCabal iopts

            Set (Right sopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for setting GHC. Use 'ghcup set ghc' instead.|])
              setGHC' sopts
            Set (Left (SetGHC sopts)) -> setGHC' sopts
            Set (Left (SetCabal sopts)) -> setCabal' sopts
            Set (Left (SetHLS sopts)) -> setHLS' sopts
            Set (Left (SetStack sopts)) -> setStack' sopts

            List ListOptions {..} ->
              runListGHC (do
                  l <- listVersions dls loTool lCriteria pfreq
                  liftIO $ printListResult lRawFormat l
                  pure ExitSuccess
                )

            Rm (Right rmopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for removing GHC. Use 'ghcup rm ghc' instead.|])
              rmGHC' rmopts
            Rm (Left (RmGHC rmopts)) -> rmGHC' rmopts
            Rm (Left (RmCabal rmopts)) -> rmCabal' rmopts
            Rm (Left (RmHLS rmopts)) -> rmHLS' rmopts
            Rm (Left (RmStack rmopts)) -> rmStack' rmopts

            DInfo ->
              do runDebugInfo $ liftE getDebugInfo
                >>= \case
                      VRight dinfo -> do
                        putStrLn $ prettyDebugInfo dinfo
                        pure ExitSuccess
                      VLeft e -> do
                        runLogger $ $(logError) $ T.pack $ prettyShow e
                        pure $ ExitFailure 8

            Compile (CompileGHC GHCCompileOptions {..}) ->
              runCompileGHC (do
                case targetGhc of
                  Left targetVer -> do
                    let vi = getVersionInfo targetVer GHC dls
                    forM_ (_viPreCompile =<< vi) $ \msg -> do
                      lift $ $(logInfo) msg
                      lift $ $(logInfo)
                        "...waiting for 5 seconds, you can still abort..."
                      liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
                  Right _ -> pure ()
                targetVer <- liftE $ compileGHC dls
                            (first (GHCTargetVersion crossTarget) targetGhc)
                            bootstrapGhc
                            jobs
                            buildConfig
                            patchDir
                            addConfArgs
                            pfreq
                let vi = getVersionInfo (_tvVersion targetVer) GHC dls
                when setCompile $ void $ liftE $
                  setGHC targetVer SetGHCOnly
                pure vi
                )
                >>= \case
                      VRight vi -> do
                        runLogger $ $(logInfo)
                          "GHC successfully compiled and installed"
                        forM_ (_viPostInstall =<< vi) $ \msg ->
                          runLogger $ $(logInfo) msg
                        pure ExitSuccess
                      VLeft (V (AlreadyInstalled _ v)) -> do
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed; if you really want to reinstall it, you may want to run 'ghcup rm ghc #{prettyVer v}' first|]
                        pure ExitSuccess
                      VLeft err@(V (BuildFailed tmpdir _)) -> do
                        case keepDirs settings of
                          Never -> runLogger $ $(logError) $ T.pack $ prettyShow err
                          _ -> runLogger ($(logError) [i|#{prettyShow err}
Check the logs at #{logsDir} and the build directory #{tmpdir} for more clues.
Make sure to clean up #{tmpdir} afterwards.|])
                        pure $ ExitFailure 9
                      VLeft e -> do
                        runLogger $ $(logError) $ T.pack $ prettyShow e
                        pure $ ExitFailure 9

            Upgrade uOpts force -> do
              target <- case uOpts of
                UpgradeInplace  -> Just <$> liftIO getExecutablePath
                (UpgradeAt p)   -> pure $ Just p
                UpgradeGHCupDir -> pure (Just (binDir </> "ghcup"))

              runUpgrade (liftE $ upgradeGHCup dls target force pfreq) >>= \case
                VRight v' -> do
                  let pretty_v = prettyVer v'
                  let vi = fromJust $ snd <$> getLatest dls GHCup
                  runLogger $ $(logInfo)
                    [i|Successfully upgraded GHCup to version #{pretty_v}|]
                  forM_ (_viPostInstall vi) $ \msg ->
                    runLogger $ $(logInfo) msg
                  pure ExitSuccess
                VLeft (V NoUpdate) -> do
                  runLogger $ $(logWarn) [i|No GHCup update available|]
                  pure ExitSuccess
                VLeft e -> do
                  runLogger $ $(logError) $ T.pack $ prettyShow e
                  pure $ ExitFailure 11

            ToolRequirements ->
              runLogger
                (runE
                  @'[NoCompatiblePlatform , DistroNotFound , NoToolRequirements]
                $ do
                    platform <- liftE getPlatform
                    req      <- getCommonRequirements platform treq ?? NoToolRequirements
                    liftIO $ T.hPutStr stdout (prettyRequirements req)
                )
                >>= \case
                      VRight _ -> pure ExitSuccess
                      VLeft  e -> do
                        runLogger $ $(logError) $ T.pack $ prettyShow e
                        pure $ ExitFailure 12

            ChangeLog ChangeLogOptions{..} -> do
              let tool = fromMaybe GHC clTool
                  ver' = maybe
                    (Right Latest)
                    (\case
                      ToolVersion tv -> Left (_tvVersion tv) -- FIXME: ugly sharing of ToolVersion
                      ToolTag     t  -> Right t
                    )
                    clToolVer
                  muri = getChangeLog dls tool ver'
              case muri of
                Nothing -> do
                  runLogger
                    ($(logWarn)
                      [i|Could not find ChangeLog for #{tool}, version #{either (T.unpack . prettyVer) show ver'}|]
                    )
                  pure ExitSuccess
                Just uri -> do
                  let uri' = T.unpack . decUTF8Safe . serializeURIRef' $ uri
                      cmd = case _rPlatform pfreq of
                              Darwin  -> "open"
                              Linux _ -> "xdg-open"
                              FreeBSD -> "xdg-open"
                              Windows -> "start"

                  if clOpen
                    then
                      exec cmd
                           [T.unpack $ decUTF8Safe $ serializeURIRef' uri]
                           Nothing
                           Nothing
                        >>= \case
                              Right _ -> pure ExitSuccess
                              Left  e -> runLogger ($(logError) [i|#{e}|])
                                >> pure (ExitFailure 13)
                    else putStrLn uri' >> pure ExitSuccess

          case res of
            ExitSuccess        -> pure ()
            ef@(ExitFailure _) -> exitWith ef
  pure ()

fromVersion :: (MonadLogger m, MonadFail m, MonadReader AppState m, MonadThrow m, MonadIO m, MonadCatch m)
            => GHCupDownloads
            -> Maybe ToolVersion
            -> Tool
            -> Excepts '[TagNotFound, NextVerNotFound, NoToolVersionSet] m (GHCTargetVersion, Maybe VersionInfo)
fromVersion av tv = fromVersion' av (toSetToolVer tv)

fromVersion' :: (MonadLogger m, MonadFail m, MonadReader AppState m, MonadThrow m, MonadIO m, MonadCatch m)
             => GHCupDownloads
             -> SetToolVersion
             -> Tool
             -> Excepts '[TagNotFound, NextVerNotFound, NoToolVersionSet] m (GHCTargetVersion, Maybe VersionInfo)
fromVersion' av SetRecommended tool =
  (\(x, y) -> (mkTVer x, Just y)) <$> getRecommended av tool
    ?? TagNotFound Recommended tool
fromVersion' av (SetToolVersion v) tool = do
  let vi = getVersionInfo (_tvVersion v) tool av
  case pvp $ prettyVer (_tvVersion v) of
    Left _ -> pure (v, vi)
    Right (PVP (major' :|[minor'])) ->
      case getLatestGHCFor (fromIntegral major') (fromIntegral minor') av of
        Just (v', vi') -> pure (GHCTargetVersion (_tvTarget v) v', Just vi')
        Nothing -> pure (v, vi)
    Right _ -> pure (v, vi)
fromVersion' av  (SetToolTag Latest) tool =
  (\(x, y) -> (mkTVer x, Just y)) <$> getLatest av tool ?? TagNotFound Latest tool
fromVersion' av (SetToolTag Recommended) tool =
  (\(x, y) -> (mkTVer x, Just y)) <$> getRecommended av tool ?? TagNotFound Recommended tool
fromVersion' av (SetToolTag (Base pvp'')) GHC =
  (\(x, y) -> (mkTVer x, Just y)) <$> getLatestBaseVersion av pvp'' ?? TagNotFound (Base pvp'') GHC
fromVersion' av SetNext tool = do
  next <- case tool of
    GHC -> do
      set <- fmap _tvVersion $ ghcSet Nothing !? NoToolVersionSet tool
      ghcs <- rights <$> lift getInstalledGHCs
      (headMay
        . tail
        . dropWhile (\GHCTargetVersion {..} -> _tvVersion /= set)
        . cycle
        . sortBy (\x y -> compare (_tvVersion x) (_tvVersion y))
        . filter (\GHCTargetVersion {..} -> _tvTarget == Nothing)
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
  let vi = getVersionInfo (_tvVersion next) tool av
  pure (next, vi)
fromVersion' _ (SetToolTag t') tool =
  throwE $ TagNotFound t' tool


printListResult :: Bool -> [ListResult] -> IO ()
printListResult raw lr = do
  -- https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8

  no_color <- isJust <$> lookupEnv "NO_COLOR"

  let
    color | raw || no_color = flip const
          | otherwise       = Pretty.color

  let
    printTag Recommended        = color Green "recommended"
    printTag Latest             = color Yellow "latest"
    printTag Prerelease         = color Red "prerelease"
    printTag (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
    printTag (UnknownTag t    ) = t
    printTag Old                = ""

  let
    rows =
      (\x -> if raw
          then x
          else [color Green "", "Tool", "Version", "Tags", "Notes"] : x
        )
        . fmap
            (\ListResult {..} ->
              let marks = if
                    | lSet       -> (color Green "✔✔")
                    | lInstalled -> (color Green "✓ ")
                    | otherwise  -> (color Red "✗ ")
              in
                (if raw then [] else [marks])
                  ++ [ fmap toLower . show $ lTool
                     , case lCross of
                       Nothing -> T.unpack . prettyVer $ lVer
                       Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
                     , intercalate "," $ (filter (/= "") . fmap printTag $ sort lTag)
                     , intercalate ","
                     $  (if hlsPowered
                          then [color Green "hls-powered"]
                          else mempty
                        )
                     ++ (if fromSrc then [color Blue "compiled"] else mempty)
                     ++ (if lStray then [color Yellow "stray"] else mempty)
                     ++ (if lNoBindist
                          then [color Red "no-bindist"]
                          else mempty
                        )
                     ]
            )
        $ lr
  let cols =
        foldr (\xs ys -> zipWith (:) xs ys) (replicate (length rows) []) rows
      lengths = fmap maximum . (fmap . fmap) strWidth $ cols
      padded  = fmap (\xs -> zipWith padTo xs lengths) rows

  forM_ padded $ \row -> putStrLn $ intercalate " " row
 where

  padTo str' x =
    let lstr = strWidth str'
        add' = x - lstr
    in  if add' < 0 then str' else str' ++ replicate add' ' '

  -- | Calculate the render width of a string, considering
  -- wide characters (counted as double width), ANSI escape codes
  -- (not counted), and line breaks (in a multi-line string, the longest
  -- line determines the width).
  strWidth :: String -> Int
  strWidth =
    maximum
      . (0 :)
      . map (foldr (\a b -> charWidth a + b) 0)
      . lines
      . stripAnsi

  -- | Strip ANSI escape sequences from a string.
  --
  -- >>> stripAnsi "\ESC[31m-1\ESC[m"
  -- "-1"
  stripAnsi :: String -> String
  stripAnsi s' =
    case
        MP.parseMaybe (many $ "" <$ MP.try ansi <|> pure <$> MP.anySingle) s'
      of
        Nothing -> error "Bad ansi escape"  -- PARTIAL: should not happen
        Just xs -> concat xs
   where
      -- This parses lots of invalid ANSI escape codes, but that should be fine
    ansi =
      MPC.string "\ESC[" *> digitSemicolons *> suffix MP.<?> "ansi" :: MP.Parsec
          Void
          String
          Char
    digitSemicolons = MP.takeWhileP Nothing (\c -> isDigit c || c == ';')
    suffix = MP.oneOf ['A', 'B', 'C', 'D', 'H', 'J', 'K', 'f', 'm', 's', 'u']

  -- | Get the designated render width of a character: 0 for a combining
  -- character, 1 for a regular character, 2 for a wide character.
  -- (Wide characters are rendered as exactly double width in apps and
  -- fonts that support it.) (From Pandoc.)
  charWidth :: Char -> Int
  charWidth c = case c of
    _ | c < '\x0300'                     -> 1
      | c >= '\x0300' && c <= '\x036F'   -> 0
      |  -- combining
        c >= '\x0370' && c <= '\x10FC'   -> 1
      | c >= '\x1100' && c <= '\x115F'   -> 2
      | c >= '\x1160' && c <= '\x11A2'   -> 1
      | c >= '\x11A3' && c <= '\x11A7'   -> 2
      | c >= '\x11A8' && c <= '\x11F9'   -> 1
      | c >= '\x11FA' && c <= '\x11FF'   -> 2
      | c >= '\x1200' && c <= '\x2328'   -> 1
      | c >= '\x2329' && c <= '\x232A'   -> 2
      | c >= '\x232B' && c <= '\x2E31'   -> 1
      | c >= '\x2E80' && c <= '\x303E'   -> 2
      | c == '\x303F'                    -> 1
      | c >= '\x3041' && c <= '\x3247'   -> 2
      | c >= '\x3248' && c <= '\x324F'   -> 1
      | -- ambiguous
        c >= '\x3250' && c <= '\x4DBF'   -> 2
      | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
      | c >= '\x4E00' && c <= '\xA4C6'   -> 2
      | c >= '\xA4D0' && c <= '\xA95F'   -> 1
      | c >= '\xA960' && c <= '\xA97C'   -> 2
      | c >= '\xA980' && c <= '\xABF9'   -> 1
      | c >= '\xAC00' && c <= '\xD7FB'   -> 2
      | c >= '\xD800' && c <= '\xDFFF'   -> 1
      | c >= '\xE000' && c <= '\xF8FF'   -> 1
      | -- ambiguous
        c >= '\xF900' && c <= '\xFAFF'   -> 2
      | c >= '\xFB00' && c <= '\xFDFD'   -> 1
      | c >= '\xFE00' && c <= '\xFE0F'   -> 1
      | -- ambiguous
        c >= '\xFE10' && c <= '\xFE19'   -> 2
      | c >= '\xFE20' && c <= '\xFE26'   -> 1
      | c >= '\xFE30' && c <= '\xFE6B'   -> 2
      | c >= '\xFE70' && c <= '\xFEFF'   -> 1
      | c >= '\xFF01' && c <= '\xFF60'   -> 2
      | c >= '\xFF61' && c <= '\x16A38'  -> 1
      | c >= '\x1B000' && c <= '\x1B001' -> 2
      | c >= '\x1D000' && c <= '\x1F1FF' -> 1
      | c >= '\x1F200' && c <= '\x1F251' -> 2
      | c >= '\x1F300' && c <= '\x1F773' -> 1
      | c >= '\x20000' && c <= '\x3FFFD' -> 2
      | otherwise                        -> 1


checkForUpdates :: ( MonadReader AppState m
                   , MonadCatch m
                   , MonadLogger m
                   , MonadThrow m
                   , MonadIO m
                   , MonadFail m
                   , MonadLogger m
                   )
                => GHCupDownloads
                -> PlatformRequest
                -> m ()
checkForUpdates dls pfreq = do
  lInstalled <- listVersions dls Nothing (Just ListInstalled) pfreq
  let latestInstalled tool = (fmap lVer . lastMay . filter (\lr -> lTool lr == tool)) lInstalled

  forM_ (getLatest dls GHCup) $ \(l, _) -> do
    (Right ghc_ver) <- pure $ version $ prettyPVP ghcUpVer
    when (l > ghc_ver)
      $ $(logWarn)
          [i|New GHCup version available: #{prettyVer l}. To upgrade, run 'ghcup upgrade'|]

  forM_ (getLatest dls GHC) $ \(l, _) -> do
    let mghc_ver = latestInstalled GHC
    forM mghc_ver $ \ghc_ver ->
      when (l > ghc_ver)
        $ $(logWarn)
            [i|New GHC version available: #{prettyVer l}. To upgrade, run 'ghcup install ghc #{prettyVer l}'|]

  forM_ (getLatest dls Cabal) $ \(l, _) -> do
    let mcabal_ver = latestInstalled Cabal
    forM mcabal_ver $ \cabal_ver ->
      when (l > cabal_ver)
        $ $(logWarn)
            [i|New Cabal version available: #{prettyVer l}. To upgrade, run 'ghcup install cabal #{prettyVer l}'|]

  forM_ (getLatest dls HLS) $ \(l, _) -> do
    let mhls_ver = latestInstalled HLS
    forM mhls_ver $ \hls_ver ->
      when (l > hls_ver)
        $ $(logWarn)
            [i|New HLS version available: #{prettyVer l}. To upgrade, run 'ghcup install hls #{prettyVer l}'|]

  forM_ (getLatest dls Stack) $ \(l, _) -> do
    let mstack_ver = latestInstalled Stack
    forM mstack_ver $ \stack_ver ->
      when (l > stack_ver)
        $ $(logWarn)
            [i|New Stack version available: #{prettyVer l}. To upgrade, run 'ghcup install stack #{prettyVer l}'|]


prettyDebugInfo :: DebugInfo -> String
prettyDebugInfo DebugInfo {..} = [i|Debug Info
==========
GHCup base dir: #{diBaseDir}
GHCup bin dir: #{diBinDir}
GHCup GHC directory: #{diGHCDir}
GHCup cache directory: #{diCacheDir}
Architecture: #{prettyShow diArch}
Platform: #{prettyShow diPlatform}
Version: #{describe_result}|]

