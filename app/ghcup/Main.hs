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
import           Data.List                      ( intercalate, sort )
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions           hiding ( str )
import           Data.Void
import           GHC.IO.Encoding
import           Haskus.Utils.Variant.Excepts
import           HPath
import           HPath.IO
import           Language.Haskell.TH
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           Safe
import           System.Console.Pretty
import           System.Environment
import           System.Exit
import           System.IO               hiding ( appendFile )
import           Text.Read               hiding ( lift )
import           URI.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC





data Options = Options
  {
  -- global options
    optVerbose   :: Bool
  , optCache     :: Bool
  , optUrlSource :: Maybe URI
  , optNoVerify  :: Bool
  , optKeepDirs  :: KeepDirs
  , optsDownloader :: Downloader
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
prettyToolVer (ToolVersion v') = T.unpack $ prettyTVer v'
prettyToolVer (ToolTag t) = show t


data InstallCommand = InstallGHC InstallOptions
                    | InstallCabal InstallOptions
                    | InstallHLS InstallOptions

data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instPlatform :: Maybe PlatformRequest
  , instBindist  :: Maybe URI
  }

data SetCommand = SetGHC SetOptions
                | SetCabal SetOptions
                | SetHLS SetOptions

data SetOptions = SetOptions
  { sToolVer :: Maybe ToolVersion
  }

data ListOptions = ListOptions
  { lTool      :: Maybe Tool
  , lCriteria  :: Maybe ListCriteria
  , lRawFormat :: Bool
  }

data RmCommand = RmGHC RmOptions
               | RmCabal Version
               | RmHLS Version

data RmOptions = RmOptions
  { ghcVer :: GHCTargetVersion
  }


data CompileCommand = CompileGHC GHCCompileOptions
                    | CompileCabal CabalCompileOptions


data GHCCompileOptions = GHCCompileOptions
  { targetVer    :: Version
  , bootstrapGhc :: Either Version (Path Abs)
  , jobs         :: Maybe Int
  , buildConfig  :: Maybe (Path Abs)
  , patchDir     :: Maybe (Path Abs)
  , crossTarget  :: Maybe Text
  , addConfArgs  :: [Text]
  }

data CabalCompileOptions = CabalCompileOptions
  { targetVer    :: Version
  , bootstrapGhc :: Either Version (Path Abs)
  , jobs         :: Maybe Int
  , buildConfig  :: Maybe (Path Abs)
  , patchDir     :: Maybe (Path Abs)
  }

data UpgradeOpts = UpgradeInplace
                 | UpgradeAt (Path Abs)
                 | UpgradeGHCupDir
                 deriving Show

data ChangeLogOptions = ChangeLogOptions
  { clOpen    :: Bool
  , clTool    :: Maybe Tool
  , clToolVer :: Maybe ToolVersion
  }


opts :: Parser Options
opts =
  Options
    <$> switch (short 'v' <> long "verbose" <> help "Enable verbosity")
    <*> switch
          (short 'c' <> long "cache" <> help "Cache downloads in ~/.ghcup/cache"
          )
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
    <*> switch
          (short 'n' <> long "no-verify" <> help
            "Skip tarball checksum verification"
          )
    <*> option
          (eitherReader keepOnParser)
          (  long "keep"
          <> metavar "<always|errors|never>"
          <> help
               "Keep build directories? (default: errors)"
          <> value Errors
          <> hidden
          )
    <*> option
          (eitherReader downloaderParser)
          (  long "downloader"
#if defined(INTERNAL_DOWNLOADER)
          <> metavar "<internal|curl|wget>"
          <> help
          "Downloader to use (default: internal)"
          <> value Internal
#else
          <> metavar "<curl|wget>"
          <> help
          "Downloader to use (default: curl)"
          <> value Curl
#endif
          <> hidden
          )
    <*> com
 where
  parseUri s' =
    bimap show id $ parseURI strictURIParserOptions (UTF8.fromString s')


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
          <$> (info
                (installParser <**> helper)
                (  progDesc "Install or update GHC/cabal"
                <> footerDoc (Just $ text installToolFooter)
                )
              )
          )
      <> command
           "set"
           ((info
              (Set <$> setParser <**> helper)
              (  progDesc "Set currently active GHC/cabal version"
              <> footerDoc (Just $ text setFooter)
              )
            )
           )
      <> command
           "rm"
           ((info
              (Rm <$> rmParser <**> helper)
              (  progDesc "Remove a GHC/cabal version"
              <> footerDoc (Just $ text rmFooter)
              )
            )
           )

      <> command
           "list"
           ((info (List <$> listOpts <**> helper)
                  (progDesc "Show available GHCs and other tools")
            )
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
           <$> (info (compileP <**> helper)
                     (progDesc "Compile a tool from source")
               )
           )
      <> commandGroup "Main commands:"
      )
    <|> subparser
          (  command
              "debug-info"
              ((\_ -> DInfo) <$> (info (helper) (progDesc "Show debug info")))
          <> command
               "tool-requirements"
               (   (\_ -> ToolRequirements)
               <$> (info (helper)
                         (progDesc "Show the requirements for ghc/cabal")
                   )
               )
          <> command
               "changelog"
               ((info
                  (fmap ChangeLog changelogP <**> helper)
                  (  progDesc "Find/show changelog"
                  <> footerDoc (Just $ text changeLogFooter)
                  )
                )
               )
          <> commandGroup "Other commands:"
          <> hidden
          )
    <|> subparser
          (  command
              "install-cabal"
              ((info
                 ((InstallCabalLegacy <$> installOpts) <**> helper)
                 (  progDesc "Install or update cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
               )
              )
          <> internal
          )
 where
  installToolFooter :: String
  installToolFooter = [s|Discussion:
  Installs GHC or cabal. When no command is given, installs GHC
  with the specified version/tag.
  It is recommended to always specify a subcommand ('ghc' or 'cabal').|]

  setFooter :: String
  setFooter = [s|Discussion:
  Sets the currently active GHC or cabal version. When no command is given,
  defaults to setting GHC with the specified version/tag (if no tag
  is given, sets GHC to 'recommended' version).
  It is recommended to always specify a subcommand ('ghc' or 'cabal').|]

  rmFooter :: String
  rmFooter = [s|Discussion:
  Remove the given GHC or cabal version. When no command is given,
  defaults to removing GHC with the specified version.
  It is recommended to always specify a subcommand ('ghc' or 'cabal').|]

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
          <$> (info
                (installOpts <**> helper)
                (  progDesc "Install GHC"
                <> footerDoc (Just $ text installGHCFooter)
                )
              )
          )
      <> command
           "cabal"
           (   InstallCabal
           <$> (info
                 (installOpts <**> helper)
                 (  progDesc "Install Cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
               )
           )
      <> command
           "hls"
           (   InstallHLS
           <$> (info
                 (installOpts <**> helper)
                 (  progDesc "Install haskell-languge-server"
                 <> footerDoc (Just $ text installHLSFooter)
                 )
               )
           )
      )
    )
    <|> (Right <$> installOpts)
 where
  installHLSFooter :: String
  installHLSFooter = [s|Discussion:
  Installs haskell-language-server binaries and wrapper
  into "~/.ghcup/bin"

Examples:
  # install recommended GHC
  ghcup install hls|]

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


installOpts :: Parser InstallOptions
installOpts =
  (\p (u, v) -> InstallOptions v p u)
    <$> (optional
          (option
            (eitherReader platformParser)
            (  short 'p'
            <> long "platform"
            <> metavar "PLATFORM"
            <> help
                 "Override for platform (triple matching ghc tarball names), e.g. x86_64-fedora27-linux"
            )
          )
        )
    <*> (   (   (,)
            <$> (optional
                  (option
                    (eitherReader bindistParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                    )
                  )
                )
            <*> (Just <$> toolVersionArgument)
            )
        <|> ((,) <$> pure Nothing <*> optional toolVersionArgument)
        )


setParser :: Parser (Either SetCommand SetOptions)
setParser =
  (Left <$> subparser
      (  command
          "ghc"
          (   SetGHC
          <$> (info
                (setOpts <**> helper)
                (  progDesc "Set GHC version"
                <> footerDoc (Just $ text setGHCFooter)
                )
              )
          )
      <> command
           "cabal"
           (   SetCabal
           <$> (info
                 (setOpts <**> helper)
                 (  progDesc "Set Cabal version"
                 <> footerDoc (Just $ text setCabalFooter)
                 )
               )
           )
      <> command
           "hls"
           (   SetHLS
           <$> (info
                 (setOpts <**> helper)
                 (  progDesc "Set haskell-language-server version"
                 <> footerDoc (Just $ text setHLSFooter)
                 )
               )
           )
      )
    )
    <|> (Right <$> setOpts)
 where
  setGHCFooter :: String
  setGHCFooter = [s|Discussion:
    Sets the the current GHC version by creating non-versioned
    symlinks for all ghc binaries of the specified version in
    "~/.ghcup/bin/<binary>".|]

  setCabalFooter :: String
  setCabalFooter = [s|Discussion:
    Sets the the current Cabal version.|]

  setHLSFooter :: String
  setHLSFooter = [s|Discussion:
    Sets the the current haskell-language-server version.|]


setOpts :: Parser SetOptions
setOpts = SetOptions <$> optional toolVersionArgument

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
    <*> (optional
          (option
            (eitherReader criteriaParser)
            (  short 'c'
            <> long "show-criteria"
            <> metavar "<installed|set>"
            <> help "Show only installed or set tool versions"
            )
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
          (RmGHC <$> (info (rmOpts <**> helper) (progDesc "Remove GHC version")))
      <> command
           "cabal"
           (   RmCabal
           <$> (info (versionParser' <**> helper)
                     (progDesc "Remove Cabal version")
               )
           )
      <> command
           "hls"
           (   RmHLS
           <$> (info (versionParser' <**> helper)
                     (progDesc "Remove haskell-language-server version")
               )
           )
      )
    )
    <|> (Right <$> rmOpts)



rmOpts :: Parser RmOptions
rmOpts = RmOptions <$> versionArgument


changelogP :: Parser ChangeLogOptions
changelogP =
  (\x y -> ChangeLogOptions x y)
    <$> switch (short 'o' <> long "open" <> help "xdg-open the changelog url")
    <*> (optional
          (option
            (eitherReader
              (\s' -> case fmap toLower s' of
                "ghc"   -> Right GHC
                "cabal" -> Right Cabal
                "ghcup" -> Right GHCup
                e       -> Left $ e
              )
            )
            (short 't' <> long "tool" <> metavar "<ghc|cabal|ghcup>" <> help
              "Open changelog for given tool (default: ghc)"
            )
          )
        )
    <*> optional toolVersionArgument

compileP :: Parser CompileCommand
compileP = subparser
  (  command
      "ghc"
      (   CompileGHC
      <$> (info
            (ghcCompileOpts <**> helper)
            (  progDesc "Compile GHC from source"
            <> footerDoc (Just $ text compileFooter)
            )
          )
      )
  <> command
       "cabal"
       (   CompileCabal
       <$> (info
             (cabalCompileOpts <**> helper)
             (  progDesc "Compile Cabal from source"
             <> footerDoc (Just $ text compileCabalFooter)
             )
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
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2
  # specify path to bootstrap ghc
  ghcup compile ghc -j 4 -v 8.4.2 -b /usr/bin/ghc-8.2.2
  # build cross compiler
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2 -x armv7-unknown-linux-gnueabihf --config $(pwd)/build.mk -- --enable-unregisterised|]
  compileCabalFooter = [i|Discussion:
  Compiles and installs the specified Cabal version
  into "~/.ghcup/bin".

Examples:
  ghcup compile cabal -j 4 -v 3.2.0.0 -b 8.6.5
  ghcup compile cabal -j 4 -v 3.2.0.0 -b /usr/bin/ghc-8.6.5|]


ghcCompileOpts :: Parser GHCCompileOptions
ghcCompileOpts =
  (\CabalCompileOptions {..} crossTarget addConfArgs -> GHCCompileOptions { .. }
    )
    <$> cabalCompileOpts
    <*> (optional
          (option
            str
            (short 'x' <> long "cross-target" <> metavar "CROSS_TARGET" <> help
              "Build cross-compiler for this platform"
            )
          )
        )
    <*> many (argument str (metavar "CONFIGURE_ARGS" <> help "Additional arguments to configure, prefix with '-- ' (longopts)"))

cabalCompileOpts :: Parser CabalCompileOptions
cabalCompileOpts =
  CabalCompileOptions
    <$> (option
          (eitherReader
            (bimap (const "Not a valid version") id . version . T.pack)
          )
          (short 'v' <> long "version" <> metavar "VERSION" <> help
            "The tool version to compile"
          )
        )
    <*> (option
          (eitherReader
            (\x ->
              (bimap (const "Not a valid version") Left . version . T.pack $ x)
                <|> (bimap show Right . parseAbs . E.encodeUtf8 . T.pack $ x)
            )
          )
          (  short 'b'
          <> long "bootstrap-ghc"
          <> metavar "BOOTSTRAP_GHC"
          <> help
               "The GHC version (or full path) to bootstrap with (must be installed)"
          )
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
            (eitherReader
              (\x ->
                bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                    String
                    (Path Abs)
              )
            )
            (short 'c' <> long "config" <> metavar "CONFIG" <> help
              "Absolute path to build config file"
            )
          )
    <*> optional
          (option
            (eitherReader
              (\x ->
                bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                    String
                    (Path Abs)
              )
            )
            (short 'p' <> long "patchdir" <> metavar "PATCH_DIR" <> help
              "Absolute path to patch directory (applied in order, uses -p1)"
            )
          )


toolVersionParser :: Parser ToolVersion
toolVersionParser = verP <|> toolP
 where
  verP = ToolVersion <$> versionParser
  toolP =
    ToolTag
      <$> (option
            (eitherReader tagEither)
            (short 't' <> long "tag" <> metavar "TAG" <> help "The target tag")
          )

-- | same as toolVersionParser, except as an argument.
toolVersionArgument :: Parser ToolVersion
toolVersionArgument =
  argument (eitherReader toolVersionEither) (metavar "VERSION|TAG")


versionArgument :: Parser GHCTargetVersion
versionArgument = argument (eitherReader tVersionEither) (metavar "VERSION")

versionParser :: Parser GHCTargetVersion
versionParser = option
  (eitherReader tVersionEither)
  (short 'v' <> long "version" <> metavar "VERSION" <> help "The target version"
  )

versionParser' :: Parser Version
versionParser' = argument
  (eitherReader (bimap show id . version . T.pack))
  (metavar "VERSION")


tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended" -> Right Recommended
  "latest"      -> Right Latest
  ('b':'a':'s':'e':'-':ver') -> case pvp (T.pack ver') of
                                  Right x -> Right (Base x)
                                  Left  _ -> Left [i|Invalid PVP version for base #{ver'}|]
  other         -> Left ([i|Unknown tag #{other}|])


tVersionEither :: String -> Either String GHCTargetVersion
tVersionEither =
  bimap (const "Not a valid version") id . MP.parse ghcTargetVerP "" . T.pack


toolVersionEither :: String -> Either String ToolVersion
toolVersionEither s' =
  bimap id ToolTag (tagEither s') <|> bimap id ToolVersion (tVersionEither s')


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
  archP = (MP.try (MP.chunk "x86_64" $> A_64)) <|> (MP.chunk "i386" $> A_32)
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
  verP :: MP.Parsec Void Text Text -> MP.Parsec Void Text Versioning
  verP suffix = do
    ver <- parseUntil suffix
    if T.null ver
      then fail "empty version"
      else do
        rest <- MP.getInput
        MP.setInput ver
        v <- versioning'
        MP.setInput rest
        pure v


bindistParser :: String -> Either String URI
bindistParser = first show . parseURI strictURIParserOptions . UTF8.fromString


toSettings :: Options -> IO Settings
toSettings Options {..} = do
  let cache      = optCache
      noVerify   = optNoVerify
      keepDirs   = optKeepDirs
      downloader = optsDownloader
      verbose    = optVerbose
  dirs <- getDirs
  pure $ Settings { .. }


upgradeOptsP :: Parser UpgradeOpts
upgradeOptsP =
  flag'
      UpgradeInplace
      (short 'i' <> long "inplace" <> help
        "Upgrade ghcup in-place (wherever it's at)"
      )
    <|> (   UpgradeAt
        <$> (option
              (eitherReader
                (\x ->
                  bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                      String
                      (Path Abs)
                )
              )
              (short 't' <> long "target" <> metavar "TARGET_DIR" <> help
                "Absolute filepath to write ghcup into"
              )
            )
        )
    <|> (pure UpgradeGHCupDir)



describe_result :: String
describe_result = $( (LitE . StringL) <$>
                     runIO (do
                             CapturedProcess{..} <- executeOut [rel|git|] ["describe"] Nothing
                             case _exitCode of
                               ExitSuccess   -> pure . T.unpack . decUTF8Safe $ _stdOut
                               ExitFailure _ -> pure numericVer
                     )
                   )


main :: IO ()
main = do
  let versionHelp = infoOption
        ( ("The GHCup Haskell installer, version " <>)
        $ (head . lines $ describe_result)
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

Report bugs at <https://gitlab.haskell.org/haskell/ghcup-hs/issues>|]

  customExecParser
      (prefs showHelpOnError)
      (info (opts <**> helper <**> versionHelp <**> numericVersionHelp <**> listCommands)
            (footerDoc (Just $ text main_footer))
      )
    >>= \opt@Options {..} -> do
          settings@Settings{dirs = Dirs{..}, ..} <- toSettings opt

          -- create ~/.ghcup dir
          createDirRecursive' baseDir

          -- logger interpreter
          logfile <- flip runReaderT settings $ initGHCupFileLogging [rel|ghcup.log|]
          let loggerConfig = LoggerConfig
                { lcPrintDebug = optVerbose
                , colorOutter  = B.hPut stderr
                , rawOutter    = appendFile logfile
                }
          let runLogger = myLoggerT loggerConfig


          -------------------------
          -- Effect interpreters --
          -------------------------

          let runInstTool' settings' =
                runLogger
                  . flip runReaderT settings'
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
                      ]

          let runInstTool = runInstTool' settings

          let
            runSetGHC =
              runLogger
                . flip runReaderT settings
                . runE
                  @'[ FileDoesNotExistError
                    , NotInstalled
                    , TagNotFound
                    ]

          let
            runSetCabal =
              runLogger
                . flip runReaderT settings
                . runE
                  @'[ NotInstalled
                    , TagNotFound
                    ]

          let
            runSetHLS =
              runLogger
                . flip runReaderT settings
                . runE
                  @'[ NotInstalled
                    , TagNotFound
                    ]

          let runListGHC = runLogger . flip runReaderT settings

          let runRm =
                runLogger . flip runReaderT settings . runE @'[NotInstalled]

          let runDebugInfo =
                runLogger
                  . flip runReaderT settings
                  . runE
                    @'[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]

          let runCompileGHC =
                runLogger
                  . flip runReaderT settings
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

          let runCompileCabal =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , BuildFailed
                      , CopyError
                      , DigestError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , PatchFailed
                      , UnknownArchive
                      , TarDirDoesNotExist
#if !defined(TAR)
                      , ArchiveResult
#endif
                      ]

          let runUpgrade =
                runLogger
                  . flip runReaderT settings
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
                        ($(logError) [i|Error determining Platform: #{e}|])
                      exitWith (ExitFailure 2)


          (GHCupInfo treq dls) <-
            ( runLogger
              . flip runReaderT settings
              . runE @'[JSONError , DownloadFailed, FileDoesNotExistError]
              $ liftE
              $ getDownloadsF (maybe GHCupURL OwnSource optUrlSource)
              )
              >>= \case
                    VRight r -> pure r
                    VLeft  e -> do
                      runLogger
                        ($(logError) [i|Error fetching download info: #{e}|])
                      exitWith (ExitFailure 2)

          case optCommand of
            Upgrade _ _ -> pure ()
            _ -> runLogger $ flip runReaderT settings $ checkForUpdates dls pfreq



          -----------------------
          -- Command functions --
          -----------------------

          let installGHC InstallOptions{..} =
                  (case instBindist of
                     Nothing -> runInstTool $ do
                       v <- liftE $ fromVersion dls instVer GHC
                       liftE $ installGHCBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                     Just uri -> runInstTool' settings{noVerify = True} $ do
                       v <- liftE $ fromVersion dls instVer GHC
                       liftE $ installGHCBindist
                         (DownloadInfo uri (Just $ RegexDir "ghc-.*") "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                    )
                    >>= \case
                          VRight _ -> do
                            runLogger $ $(logInfo) ("GHC installation successful")
                            pure ExitSuccess
                          VLeft (V (AlreadyInstalled _ v)) -> do
                            runLogger $ $(logWarn)
                              [i|GHC ver #{prettyVer v} already installed, you may want to run 'ghcup rm ghc #{prettyVer v}' first|]
                            pure ExitSuccess
                          VLeft (V (BuildFailed tmpdir e)) -> do
                            case keepDirs of
                              Never -> runLogger ($(logError) [i|Build failed with #{e}|])
                              _ -> runLogger ($(logError) [i|Build failed with #{e}
    Check the logs at #{logsDir} and the build directory #{tmpdir} for more clues.
    Make sure to clean up #{tmpdir} afterwards.|])
                            pure $ ExitFailure 3
                          VLeft (V NoDownload) -> do

                            runLogger $ do
                              case instVer of
                                Just iver -> $(logError) [i|No available GHC version for #{prettyToolVer iver}|]
                                Nothing -> $(logError) [i|No available recommended GHC version|]
                            pure $ ExitFailure 3
                          VLeft e -> do
                            runLogger $ do
                              $(logError) [i|#{e}|]
                              $(logError) [i|Also check the logs in #{logsDir}|]
                            pure $ ExitFailure 3


          let installCabal InstallOptions{..} =
                (case instBindist of
                   Nothing -> runInstTool $ do
                     v <- liftE $ fromVersion dls instVer Cabal
                     liftE $ installCabalBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                   Just uri -> runInstTool' settings{noVerify = True} $ do
                     v <- liftE $ fromVersion dls instVer Cabal
                     liftE $ installCabalBindist
                         (DownloadInfo uri Nothing "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                  )
                  >>= \case
                        VRight _ -> do
                          runLogger $ $(logInfo) ("Cabal installation successful")
                          pure ExitSuccess
                        VLeft (V (AlreadyInstalled _ v)) -> do
                          runLogger $ $(logWarn)
                            [i|Cabal ver #{prettyVer v} already installed, you may want to run 'ghcup rm cabal #{prettyVer v}' first|]
                          pure ExitSuccess
                        VLeft (V NoDownload) -> do

                          runLogger $ do
                            case instVer of
                              Just iver -> $(logError) [i|No available Cabal version for #{prettyToolVer iver}|]
                              Nothing -> $(logError) [i|No available recommended Cabal version|]
                          pure $ ExitFailure 4
                        VLeft e -> do
                          runLogger $ do
                            $(logError) [i|#{e}|]
                            $(logError) [i|Also check the logs in #{logsDir}|]
                          pure $ ExitFailure 4

          let installHLS InstallOptions{..} =
                (case instBindist of
                   Nothing -> runInstTool $ do
                     v <- liftE $ fromVersion dls instVer HLS
                     liftE $ installHLSBin dls (_tvVersion v) (fromMaybe pfreq instPlatform)
                   Just uri -> runInstTool' settings{noVerify = True} $ do
                     v <- liftE $ fromVersion dls instVer HLS
                     liftE $ installHLSBindist
                         (DownloadInfo uri Nothing "")
                         (_tvVersion v)
                         (fromMaybe pfreq instPlatform)
                  )
                  >>= \case
                        VRight _ -> do
                          runLogger $ $(logInfo) ("HLS installation successful")
                          pure ExitSuccess
                        VLeft (V (AlreadyInstalled _ v)) -> do
                          runLogger $ $(logWarn)
                            [i|HLS ver #{prettyVer v} already installed, you may want to run 'ghcup rm hls #{prettyVer v}' first|]
                          pure ExitSuccess
                        VLeft (V NoDownload) -> do

                          runLogger $ do
                            case instVer of
                              Just iver -> $(logError) [i|No available HLS version for #{prettyToolVer iver}|]
                              Nothing -> $(logError) [i|No available recommended HLS version|]
                          pure $ ExitFailure 4
                        VLeft e -> do
                          runLogger $ do
                            $(logError) [i|#{e}|]
                            $(logError) [i|Also check the logs in #{logsDir}|]
                          pure $ ExitFailure 4


          let setGHC' SetOptions{..} =
                (runSetGHC $ do
                    v <- liftE $ fromVersion dls sToolVer GHC
                    liftE $ setGHC v SetGHCOnly
                  )
                  >>= \case
                        VRight (GHCTargetVersion{..}) -> do
                          runLogger
                            $ $(logInfo)
                                [i|GHC #{prettyVer _tvVersion} successfully set as default version#{maybe "" (" for cross target " <>) _tvTarget}|]
                          pure ExitSuccess
                        VLeft e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 5

          let setCabal' SetOptions{..} =
                (runSetCabal $ do
                    v <- liftE $ fromVersion dls sToolVer Cabal
                    liftE $ setCabal (_tvVersion v)
                  )
                  >>= \case
                        VRight _ -> pure ExitSuccess
                        VLeft  e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 14

          let setHLS' SetOptions{..} =
                (runSetHLS $ do
                    v <- liftE $ fromVersion dls sToolVer HLS
                    liftE $ setHLS (_tvVersion v)
                  )
                  >>= \case
                        VRight _ -> pure ExitSuccess
                        VLeft  e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 14

          let rmGHC' RmOptions{..} =
                (runRm $ do
                    liftE $ rmGHCVer ghcVer
                  )
                  >>= \case
                        VRight _ -> pure ExitSuccess
                        VLeft  e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 7

          let rmCabal' tv =
                (runRm $ do
                    liftE $ rmCabalVer tv
                  )
                  >>= \case
                        VRight _ -> pure ExitSuccess
                        VLeft  e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 15

          let rmHLS' tv =
                (runRm $ do
                    liftE $ rmHLSVer tv
                  )
                  >>= \case
                        VRight _ -> pure ExitSuccess
                        VLeft  e -> do
                          runLogger ($(logError) [i|#{e}|])
                          pure $ ExitFailure 15


          res <- case optCommand of
#if defined(BRICK)
            Interactive -> liftIO $ brickMain settings optUrlSource loggerConfig dls pfreq >> pure ExitSuccess
#endif
            Install (Right iopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for installing GHC. Use 'ghcup install ghc' instead.|])
              installGHC iopts
            Install (Left (InstallGHC iopts)) -> installGHC iopts
            Install (Left (InstallCabal iopts)) -> installCabal iopts
            Install (Left (InstallHLS iopts)) -> installHLS iopts
            InstallCabalLegacy iopts -> do
              runLogger ($(logWarn) [i|This is an old-style command for installing cabal. Use 'ghcup install cabal' instead.|])
              installCabal iopts

            Set (Right sopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for setting GHC. Use 'ghcup set ghc' instead.|])
              setGHC' sopts
            Set (Left (SetGHC sopts)) -> setGHC' sopts
            Set (Left (SetCabal sopts)) -> setCabal' sopts
            Set (Left (SetHLS sopts)) -> setHLS' sopts

            List (ListOptions {..}) ->
              (runListGHC $ do
                  l <- listVersions dls lTool lCriteria pfreq
                  liftIO $ printListResult lRawFormat l
                  pure ExitSuccess
                )

            Rm (Right rmopts) -> do
              runLogger ($(logWarn) [i|This is an old-style command for removing GHC. Use 'ghcup rm ghc' instead.|])
              rmGHC' rmopts
            Rm (Left (RmGHC rmopts)) -> rmGHC' rmopts
            Rm (Left (RmCabal rmopts)) -> rmCabal' rmopts
            Rm (Left (RmHLS rmopts)) -> rmHLS' rmopts

            DInfo ->
              do
                  (runDebugInfo $ liftE $ getDebugInfo)
                >>= \case
                      VRight dinfo -> do
                        putStrLn $ prettyDebugInfo dinfo
                        pure ExitSuccess
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 8

            Compile (CompileGHC GHCCompileOptions {..}) ->
              (runCompileGHC $ liftE $ compileGHC dls
                                                  (GHCTargetVersion crossTarget targetVer)
                                                  bootstrapGhc
                                                  jobs
                                                  buildConfig
                                                  patchDir
                                                  addConfArgs
                                                  pfreq
                )
                >>= \case
                      VRight _ -> do
                        runLogger $ $(logInfo)
                          ("GHC successfully compiled and installed")
                        pure ExitSuccess
                      VLeft (V (AlreadyInstalled _ v)) -> do
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed, you may want to run 'ghcup rm ghc #{prettyVer v}' first|]
                        pure ExitSuccess
                      VLeft (V (BuildFailed tmpdir e)) -> do
                        case keepDirs of
                          Never -> runLogger ($(logError) [i|Build failed with #{e}
Check the logs at #{logsDir}|])
                          _ -> runLogger ($(logError) [i|Build failed with #{e}
Check the logs at #{logsDir} and the build directory #{tmpdir} for more clues.
Make sure to clean up #{tmpdir} afterwards.|])
                        pure $ ExitFailure 9
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 9

            Compile (CompileCabal CabalCompileOptions {..}) ->
              (runCompileCabal $ do
                  liftE $ compileCabal dls targetVer bootstrapGhc jobs patchDir pfreq
                )
                >>= \case
                      VRight _ -> do
                        runLogger
                          ($(logInfo)
                            "Cabal successfully compiled and installed"
                          )
                        pure ExitSuccess
                      VLeft (V (BuildFailed tmpdir e)) -> do
                        case keepDirs of
                          Never -> runLogger ($(logError) [i|Build failed with #{e}|])
                          _ -> runLogger ($(logError) [i|Build failed with #{e}
Check the logs at #{logsDir} and the build directory #{tmpdir} for more clues.
Make sure to clean up #{tmpdir} afterwards.|])
                        pure $ ExitFailure 10
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 10

            Upgrade (uOpts) force -> do
              target <- case uOpts of
                UpgradeInplace -> do
                  efp <- liftIO $ getExecutablePath
                  p   <- parseAbs . E.encodeUtf8 . T.pack $ efp
                  pure $ Just p
                (UpgradeAt p)   -> pure $ Just p
                UpgradeGHCupDir -> pure (Just (binDir </> [rel|ghcup|]))

              (runUpgrade $ (liftE $ upgradeGHCup dls target force pfreq)) >>= \case
                VRight v' -> do
                  let pretty_v = prettyVer v'
                  runLogger $ $(logInfo)
                    [i|Successfully upgraded GHCup to version #{pretty_v}|]
                  pure ExitSuccess
                VLeft (V NoUpdate) -> do
                  runLogger $ $(logWarn) [i|No GHCup update available|]
                  pure ExitSuccess
                VLeft e -> do
                  runLogger ($(logError) [i|#{e}|])
                  pure $ ExitFailure 11

            ToolRequirements ->
              ( runLogger
                $ runE
                  @'[NoCompatiblePlatform , DistroNotFound , NoToolRequirements]
                $ do
                    platform <- liftE $ getPlatform
                    req      <-
                      (getCommonRequirements platform $ treq)
                        ?? NoToolRequirements
                    liftIO $ T.hPutStr stdout (prettyRequirements req)
                )
                >>= \case
                      VRight _ -> pure ExitSuccess
                      VLeft  e -> do
                        runLogger
                          ($(logError)
                            [i|Error getting tool requirements: #{e}|]
                          )
                        pure $ ExitFailure 12

            ChangeLog (ChangeLogOptions {..}) -> do
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

                  if clOpen
                    then
                      exec cmd
                           True
                           [serializeURIRef' uri]
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


fromVersion :: Monad m
            => GHCupDownloads
            -> Maybe ToolVersion
            -> Tool
            -> Excepts '[TagNotFound] m GHCTargetVersion
fromVersion av Nothing tool =
  mkTVer <$> getRecommended av tool ?? TagNotFound Recommended tool
fromVersion av (Just (ToolVersion v)) _ = do
  case pvp $ prettyVer (_tvVersion v) of
    Left _ -> pure v
    Right (PVP (major' :|[minor'])) ->
      case getLatestGHCFor (fromIntegral major') (fromIntegral minor') av of
        Just v' -> pure $ GHCTargetVersion (_tvTarget v) v'
        Nothing -> pure v
    Right _ -> pure v
fromVersion av (Just (ToolTag Latest)) tool =
  mkTVer <$> getLatest av tool ?? TagNotFound Latest tool
fromVersion av (Just (ToolTag Recommended)) tool =
  mkTVer <$> getRecommended av tool ?? TagNotFound Recommended tool
fromVersion av (Just (ToolTag (Base pvp''))) GHC =
  mkTVer <$> getLatestBaseVersion av pvp'' ?? TagNotFound (Base pvp'') GHC
fromVersion _ (Just (ToolTag t')) tool =
  throwE $ TagNotFound t' tool


printListResult :: Bool -> [ListResult] -> IO ()
printListResult raw lr = do
  -- https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8

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
                     , intercalate "," $ (fmap printTag $ sort lTag)
                     , intercalate ","
                     $  (if hlsPowered
                          then [color' Green "hls-powered"]
                          else mempty
                        )
                     ++ (if fromSrc then [color' Blue "compiled"] else mempty)
                     ++ (if lStray then [color' Yellow "stray"] else mempty)
                     ++ (if lNoBindist
                          then [color' Red "no-bindist"]
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
  printTag Recommended        = color' Green "recommended"
  printTag Latest             = color' Yellow "latest"
  printTag Prerelease         = color' Red "prerelease"
  printTag (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
  printTag (UnknownTag t    ) = t

  color' = case raw of
    True  -> flip const
    False -> color

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


checkForUpdates :: (MonadReader Settings m, MonadCatch m, MonadLogger m, MonadThrow m, MonadIO m, MonadFail m, MonadLogger m)
                => GHCupDownloads
                -> PlatformRequest
                -> m ()
checkForUpdates dls pfreq = do
  forM_ (getLatest dls GHCup) $ \l -> do
    (Right ghc_ver) <- pure $ version $ prettyPVP ghcUpVer
    when (l > ghc_ver)
      $ $(logWarn)
          [i|New GHCup version available: #{prettyVer l}. To upgrade, run 'ghcup upgrade'|]

  forM_ (getLatest dls GHC) $ \l -> do
    mghc_ver <- latestInstalled GHC
    forM mghc_ver $ \ghc_ver ->
      when (l > ghc_ver)
        $ $(logWarn)
            [i|New GHC version available: #{prettyVer l}. To upgrade, run 'ghcup install ghc #{prettyVer l}'|]

  forM_ (getLatest dls Cabal) $ \l -> do
    mcabal_ver <- latestInstalled Cabal
    forM mcabal_ver $ \cabal_ver ->
      when (l > cabal_ver)
        $ $(logWarn)
            [i|New Cabal version available: #{prettyVer l}. To upgrade, run 'ghcup install cabal #{prettyVer l}'|]

  forM_ (getLatest dls HLS) $ \l -> do
    mcabal_ver <- latestInstalled HLS
    forM mcabal_ver $ \cabal_ver ->
      when (l > cabal_ver)
        $ $(logWarn)
            [i|New HLS version available: #{prettyVer l}. To upgrade, run 'ghcup install hls #{prettyVer l}'|]

 where
  latestInstalled tool = (fmap lVer . lastMay)
    <$> (listVersions dls (Just tool) (Just ListInstalled) pfreq)


prettyDebugInfo :: DebugInfo -> String
prettyDebugInfo DebugInfo {..} = [i|Debug Info
==========
GHCup base dir: #{toFilePath diBaseDir}
GHCup bin dir: #{toFilePath diBinDir}
GHCup GHC directory: #{toFilePath diGHCDir}
GHCup cache directory: #{toFilePath diCacheDir}
Architecture: #{prettyArch diArch}
Platform: #{prettyPlatform diPlatform}
Version: #{describe_result}|]

