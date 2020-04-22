{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Requirements
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Logger
import           GHCup.Utils.Prelude
import           GHCup.Version

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
import           Data.Versions
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
import           Text.Read
import           Text.Layout.Table
import           URI.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP





data Options = Options
  {
  -- global options
    optVerbose   :: Bool
  , optCache     :: Bool
  , optUrlSource :: Maybe URI
  , optNoVerify  :: Bool
  -- commands
  , optCommand   :: Command
  }

data Command
  = Install InstallOptions
  | InstallCabal InstallOptions
  | SetGHC SetGHCOptions
  | List ListOptions
  | Rm RmOptions
  | DInfo
  | Compile CompileCommand
  | Upgrade UpgradeOpts Bool
  | ToolRequirements
  | ChangeLog ChangeLogOptions

data ToolVersion = ToolVersion Version
                 | ToolTag Tag

prettyToolVer :: ToolVersion -> String
prettyToolVer (ToolVersion v') = T.unpack $ prettyVer v'
prettyToolVer (ToolTag t) = show t


data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instPlatform :: Maybe PlatformRequest
  }

data SetGHCOptions = SetGHCOptions
  { ghcVer :: Maybe ToolVersion
  }

data ListOptions = ListOptions
  { lTool      :: Maybe Tool
  , lCriteria  :: Maybe ListCriteria
  , lRawFormat :: Bool
  }

data RmOptions = RmOptions
  { ghcVer :: Version
  }


data CompileCommand = CompileGHC CompileOptions
                    | CompileCabal CompileOptions


data CompileOptions = CompileOptions
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
    <*> com
 where
  parseUri s' =
    bimap show id $ parseURI strictURIParserOptions (UTF8.fromString s')


com :: Parser Command
com =
  subparser
      (  command
          "install"
          ((info
             ((Install <$> installOpts) <**> helper)
             (  progDesc "Install or update GHC"
             <> footerDoc (Just $ text installFooter)
             )
           )
          )
      <> command
           "set"
           (   SetGHC
           <$> (info
                 (setGHCOpts <**> helper)
                 (  progDesc "Set currently active GHC version"
                 <> footerDoc (Just $ text setFooter)
                 )
               )
           )
      <> command
           "rm"
           (   Rm
           <$> (info (rmOpts <**> helper) (progDesc "Remove a GHC version"))
           )

      <> command
           "install-cabal"
           ((info
              ((InstallCabal <$> installOpts) <**> helper)
              (  progDesc "Install or update cabal"
              <> footerDoc (Just $ text installCabalFooter)
              )
            )
           )
      <> command
           "list"
           (   List
           <$> (info (listOpts <**> helper)
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
               ((info (fmap ChangeLog changelogP <**> helper)
                      (progDesc "Find/show changelog"
                      <> footerDoc (Just $ text changeLogFooter)
                      )
                )
               )
          <> commandGroup "Other commands:"
          <> hidden
          )
 where
  installFooter = [i|Discussion:
  Installs the specified GHC version (or a recommended default one) into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".|]
  setFooter = [i|Discussion:
  Sets the the current GHC version by creating non-versioned
  symlinks for all ghc binaries of the specified version in
  "~/.ghcup/bin/<binary>".|]
  installCabalFooter = [i|Discussion:
  Installs the specified cabal-install version (or a recommended default one)
  into "~/.ghcup/bin", so it can be overwritten by later
  "cabal install cabal-install", which installs into "~/.cabal/bin" by
  default. Make sure to set up your PATH appropriately, so the cabal
  installation takes precedence.|]
  changeLogFooter = [i|Discussion:
  By default returns the URI of the ChangeLog of the latest GHC release.
  Pass '-o' to automatically open via xdg-open.|]


installOpts :: Parser InstallOptions
installOpts =
  (flip InstallOptions)
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
    <*> optional toolVersionArgument


setGHCOpts :: Parser SetGHCOptions
setGHCOpts = SetGHCOptions <$> optional toolVersionArgument

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
            (compileOpts <**> helper)
            (  progDesc "Compile GHC from source"
            <> footerDoc (Just $ text compileFooter)
            )
          )
      )
  <> command
       "cabal"
       (   CompileCabal
       <$> (info
             (compileOpts <**> helper)
             (  progDesc "Compile Cabal from source"
             <> footerDoc (Just $ text compileCabalFooter)
             )
           )
       )
  )
 where
  compileFooter = [i|Discussion:
  Compiles and installs the specified GHC version into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".

Examples:
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2
  ghcup compile ghc -j 4 -v 8.4.2 -b /usr/bin/ghc-8.2.2|]
  compileCabalFooter = [i|Discussion:
  Compiles and installs the specified Cabal version
  into "~/.ghcup/bin".

Examples:
  ghcup compile cabal -j 4 -v 3.2.0.0 -b 8.6.5
  ghcup compile cabal -j 4 -v 3.2.0.0 -b /usr/bin/ghc-8.6.5|]



compileOpts :: Parser CompileOptions
compileOpts =
  CompileOptions
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


versionArgument :: Parser Version
versionArgument = argument (eitherReader versionEither) (metavar "VERSION")

versionParser :: Parser Version
versionParser = option
  (eitherReader versionEither)
  (short 'v' <> long "version" <> metavar "VERSION" <> help "The target version"
  )

tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended" -> Right Recommended
  "latest"      -> Right Latest
  ('b':'a':'s':'e':'-':ver') -> case pvp (T.pack ver') of
                                  Right x -> Right (Base x)
                                  Left  _ -> Left [i|Invalid PVP version for base #{ver'}|]
  other         -> Left ([i|Unknown tag #{other}|])

versionEither :: String -> Either String Version
versionEither s' =
  -- 'version' is a bit too lax and will parse typoed tags
                   case readMaybe ((: []) . head $ s') :: Maybe Int of
  Just _  -> bimap (const "Not a valid version") id . version . T.pack $ s'
  Nothing -> Left "Not a valid version"

toolVersionEither :: String -> Either String ToolVersion
toolVersionEither s' =
  bimap id ToolTag (tagEither s') <|> bimap id ToolVersion (versionEither s')


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

  choice' []       = fail "Empty list"
  choice' [x     ] = x
  choice' (x : xs) = MP.try x <|> choice' xs

  parseUntil :: MP.Parsec Void Text Text -> MP.Parsec Void Text Text
  parseUntil p = do
    (MP.try (MP.lookAhead p) $> mempty)
      <|> (do
            c  <- T.singleton <$> MP.anySingle
            c2 <- parseUntil p
            pure (c `mappend` c2)
          )


toSettings :: Options -> Settings
toSettings Options {..} =
  let cache    = optCache
      noVerify = optNoVerify
  in  Settings { .. }


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

  let main_footer = [i|Discussion:
  ghcup installs the Glasgow Haskell Compiler from the official
  release channels, enabling you to easily switch between different
  versions.

Report bugs at <https://gitlab.haskell.org/haskell/ghcup-hs/issues>|]

  customExecParser
      (prefs showHelpOnError)
      (info (opts <**> helper <**> versionHelp <**> numericVersionHelp)
            (footerDoc (Just $ text main_footer))
      )
    >>= \opt@Options {..} -> do
          let settings = toSettings opt

          -- create ~/.ghcup dir
          ghcdir <- ghcupBaseDir
          createDirIfMissing newDirPerms ghcdir

          -- logger interpreter
          logfile <- initGHCupFileLogging [rel|ghcup.log|]
          let runLogger = myLoggerT LoggerConfig
                { lcPrintDebug = optVerbose
                , colorOutter  = B.hPut stderr
                , rawOutter    = appendFile logfile
                }

          -- wrapper to run effects with settings
          let runInstTool =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , UnknownArchive
                      , DistroNotFound
                      , FileDoesNotExistError
                      , CopyError
                      , NoCompatibleArch
                      , NoDownload
                      , NotInstalled
                      , NoCompatiblePlatform
                      , BuildFailed
                      , TagNotFound
                      , DigestError
                      , DownloadFailed
                      ]

          let
            runSetGHC =
              runLogger
                . flip runReaderT settings
                . runE
                  @'[ FileDoesNotExistError
                    , NotInstalled
                    , TagNotFound
                    , TagNotFound
                    ]

          let runListGHC = runE @'[] . runLogger

          let runRmGHC =
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
                      , DistroNotFound
                      , DownloadFailed
                      , GHCupSetError
                      , NoCompatibleArch
                      , NoCompatiblePlatform
                      , NoDownload
                      , NotFoundInPATH
                      , PatchFailed
                      , UnknownArchive
                      ]

          let runCompileCabal =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ BuildFailed
                      , DigestError
                      , DistroNotFound
                      , DownloadFailed
                      , NoCompatibleArch
                      , NoCompatiblePlatform
                      , NoDownload
                      , PatchFailed
                      , UnknownArchive
                      ]

          let runUpgrade =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ DigestError
                      , DistroNotFound
                      , NoCompatiblePlatform
                      , NoCompatibleArch
                      , NoDownload
                      , NoUpdate
                      , FileDoesNotExistError
                      , CopyError
                      , DownloadFailed
                      ]

          (GHCupInfo treq dls) <-
            ( runLogger
              . flip runReaderT settings
              . runE @'[JSONError , DownloadFailed]
              $ liftE
              $ getDownloads (maybe GHCupURL OwnSource optUrlSource)
              )
              >>= \case
                    VRight r -> pure r
                    VLeft  e -> do
                      runLogger
                        ($(logError) [i|Error fetching download info: #{e}|])
                      exitWith (ExitFailure 2)
          runLogger $ checkForUpdates dls

          res <- case optCommand of
            Install (InstallOptions {..}) ->
              (runInstTool $ do
                  v <- liftE $ fromVersion dls instVer GHC
                  liftE $ installGHCBin dls v instPlatform
                )
                >>= \case
                      VRight _ -> do
                        runLogger $ $(logInfo) ("GHC installation successful")
                        pure ExitSuccess
                      VLeft (V (AlreadyInstalled _ v)) -> do
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed|]
                        pure ExitSuccess
                      VLeft (V (BuildFailed tmpdir e)) -> do
                        runLogger
                          ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.|]
                          )
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
                          $(logError) [i|Also check the logs in ~/.ghcup/logs|]
                        pure $ ExitFailure 3
            InstallCabal (InstallOptions {..}) ->
              (runInstTool $ do
                  v <- liftE $ fromVersion dls instVer Cabal
                  liftE $ installCabalBin dls v instPlatform
                )
                >>= \case
                      VRight _ -> do
                        runLogger $ $(logInfo) ("Cabal installation successful")
                        pure ExitSuccess
                      VLeft (V (AlreadyInstalled _ v)) -> do
                        runLogger $ $(logWarn)
                          [i|Cabal ver #{prettyVer v} already installed|]
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
                          $(logError) [i|Also check the logs in ~/.ghcup/logs|]
                        pure $ ExitFailure 4

            SetGHC (SetGHCOptions {..}) ->
              (runSetGHC $ do
                  v <- liftE $ fromVersion dls ghcVer GHC
                  liftE $ setGHC v SetGHCOnly
                )
                >>= \case
                      VRight v -> do
                        runLogger
                          $ $(logInfo)
                              [i|GHC #{prettyVer v} successfully set as default version|]
                        pure ExitSuccess
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 5

            List (ListOptions {..}) ->
              (runListGHC $ do
                  l <- listVersions dls lTool lCriteria
                  pure l
                )
                >>= \case
                      VRight r -> do
                        liftIO $ printListResult lRawFormat r
                        pure ExitSuccess
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 6

            Rm (RmOptions {..}) ->
              (runRmGHC $ do
                  liftE $ rmGHCVer ghcVer
                )
                >>= \case
                      VRight _ -> pure ExitSuccess
                      VLeft  e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 7

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

            Compile (CompileGHC CompileOptions {..}) ->
              (runCompileGHC $ liftE $ compileGHC dls
                                                  targetVer
                                                  bootstrapGhc
                                                  jobs
                                                  buildConfig
                                                  patchDir
                )
                >>= \case
                      VRight _ -> do
                        runLogger $ $(logInfo)
                          ("GHC successfully compiled and installed")
                        pure ExitSuccess
                      VLeft (V (AlreadyInstalled _ v)) -> do
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed|]
                        pure ExitSuccess
                      VLeft (V (BuildFailed tmpdir e)) -> do
                        runLogger
                          ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.
Make sure to clean up #{tmpdir} afterwards.|]
                          )
                        pure $ ExitFailure 9
                      VLeft e -> do
                        runLogger ($(logError) [i|#{e}|])
                        pure $ ExitFailure 9

            Compile (CompileCabal CompileOptions {..}) ->
              (runCompileCabal $ do
                  liftE $ compileCabal dls targetVer bootstrapGhc jobs patchDir
                )
                >>= \case
                      VRight _ -> do
                        runLogger
                          ($(logInfo)
                            "Cabal successfully compiled and installed"
                          )
                        pure ExitSuccess
                      VLeft (V (BuildFailed tmpdir e)) -> do
                        runLogger
                          ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.|]
                          )
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
                UpgradeGHCupDir -> do
                  bdir <- liftIO $ ghcupBinDir
                  pure (Just (bdir </> [rel|ghcup|]))

              (runUpgrade $ (liftE $ upgradeGHCup dls target force)) >>= \case
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
                      ToolVersion tv -> Left tv
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
                  if clOpen
                    then
                      exec "xdg-open"
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
            -> Excepts '[TagNotFound] m Version
fromVersion av Nothing tool =
  getRecommended av tool ?? TagNotFound Recommended tool
fromVersion av (Just (ToolVersion v)) _ = do
  case pvp $ prettyVer v of
    Left _ -> pure v
    Right (PVP (major' :|[minor'])) ->
      case getLatestGHCFor (fromIntegral major') (fromIntegral minor') av of
        Just v' -> pure v'
        Nothing -> pure v
    Right _ -> pure v
fromVersion av (Just (ToolTag Latest)) tool =
  getLatest av tool ?? TagNotFound Latest tool
fromVersion av (Just (ToolTag Recommended)) tool =
  getRecommended av tool ?? TagNotFound Recommended tool
fromVersion av (Just (ToolTag (Base pvp''))) GHC =
  getLatestBaseVersion av pvp'' ?? TagNotFound (Base pvp'') GHC
fromVersion _ (Just (ToolTag t')) tool =
  throwE $ TagNotFound t' tool


printListResult :: Bool -> [ListResult] -> IO ()
printListResult raw lr = do
  -- https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8

  let
    formatted =
      gridString
          (  (if raw then [] else [column expand left def def])
          ++ [ column expand left def def
             , column expand left def def
             , column expand left def def
             , column expand left def def
             ]
          )
        . (\x -> if raw
            then x
            else [color Green "", "Tool", "Version", "Tags", "Notes"] : x
          )
        $ fmap
            (\ListResult {..} ->
              let marks = if
                    | lSet       -> (color Green "✔✔")
                    | lInstalled -> (color Green "✓")
                    | otherwise  -> (color Red "✗")
              in  (if raw then [] else [marks])
                    ++ [ fmap toLower . show $ lTool
                       , T.unpack . prettyVer $ lVer
                       , intercalate "," $ (fmap printTag $ sort lTag)
                       , intercalate ","
                       $  (if fromSrc then [color' Blue "compiled"] else mempty)
                       ++ (if lStray then [color' Blue "stray"] else mempty)
                       ]
            )
            lr
  putStrLn $ formatted
 where
  printTag Recommended        = color' Green "recommended"
  printTag Latest             = color' Yellow "latest"
  printTag (Base pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
  printTag (UnknownTag t    ) = t
  color' = case raw of
    True  -> flip const
    False -> color

checkForUpdates :: (MonadThrow m, MonadIO m, MonadFail m, MonadLogger m)
                => GHCupDownloads
                -> m ()
checkForUpdates dls = do
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
            [i|New GHC version available: #{prettyVer l}. To upgrade, run 'ghcup install #{prettyVer l}'|]

  forM_ (getLatest dls Cabal) $ \l -> do
    mcabal_ver <- latestInstalled Cabal
    forM mcabal_ver $ \cabal_ver ->
      when (l > cabal_ver)
        $ $(logWarn)
            [i|New Cabal version available: #{prettyVer l}. To upgrade, run 'ghcup install-cabal #{prettyVer l}'|]

 where
  latestInstalled tool = (fmap lVer . lastMay)
    <$> (listVersions dls (Just tool) (Just ListInstalled))


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
 where
  prettyArch :: Architecture -> String
  prettyArch A_64 = "amd64"
  prettyArch A_32 = "i386"
  prettyPlatform :: PlatformResult -> String
  prettyPlatform PlatformResult { _platform = plat, _distroVersion = Just v' }
    = show plat <> ", " <> show v'
  prettyPlatform PlatformResult { _platform = plat, _distroVersion = Nothing }
    = show plat

