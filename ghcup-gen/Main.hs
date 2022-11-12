{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main where

import           GHCup.Types
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Utils.Dirs
import           GHCup.Utils.Logger
import           GHCup.Types.JSON               ( )

import           Control.Exception              ( displayException )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Control.Monad.IO.Class
import           Data.Char                      ( toLower )
import           Data.Maybe
#if !MIN_VERSION_base(4,13,0)
import           Data.Semigroup                 ( (<>) )
#endif
import           Options.Applicative     hiding ( style )
import           Haskus.Utils.Variant.Excepts
import           System.Console.Pretty
import           System.Environment
import           System.Exit
import           System.IO                      ( stderr )
import           Text.Regex.Posix
import           Generate
import           Validate
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Text.IO                  as T
import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import qualified Data.Yaml.Aeson               as Y


data Options = Options
  { optCommand :: Command
  }


formatParser :: Parser Format
formatParser =
    option
    (eitherReader formatP)
          (long "format" <> metavar "FORMAT" <> help
            "Which format to use (JSON | YAML). Yaml is default."
            <> value FormatJSON
          )
 where
  formatP :: String -> Either String Format
  formatP s' | t == T.pack "json" = Right FormatJSON
             | t == T.pack "yaml" = Right FormatYAML
             | t == T.pack "yml"  = Right FormatYAML
             | otherwise          = Left ("Unknown format value: " <> s')
    where t = T.toLower (T.pack s')


data Command = ValidateYAML ValidateYAMLOpts
             | ValidateTarballs ValidateYAMLOpts TarballFilter
             | GenerateHlsGhc ValidateYAMLOpts Format Output
             | GenerateToolTable ValidateYAMLOpts Output
             | GenerateSystemDepsInfo ValidateYAMLOpts Output


fileOutput :: Parser Output
fileOutput =
  FileOutput
    <$> strOption
          (long "output-file" <> short 'o' <> metavar "FILENAME" <> help
            "Output file to write to"
          )

stdOutput :: Parser Output
stdOutput = flag'
  StdOut
  (short 'o' <> long "stdout" <> help "Output to stdout (default)")

outputP :: Parser Output
outputP = fileOutput <|> stdOutput

data Input
  = FileInput FilePath -- optsparse-applicative doesn't handle ByteString correctly anyway
  | StdInput

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
          (long "file" <> short 'f' <> metavar "FILENAME" <> help
            "Input file to validate"
          )

stdInput :: Parser Input
stdInput = flag'
  StdInput
  (short 'i' <> long "stdin" <> help "Validate from stdin (default)")

inputP :: Parser Input
inputP = fileInput <|> stdInput

data ValidateYAMLOpts = ValidateYAMLOpts
  { vInput :: Maybe Input
  }

validateYAMLOpts :: Parser ValidateYAMLOpts
validateYAMLOpts = ValidateYAMLOpts <$> optional inputP

tarballFilterP :: Parser TarballFilter
tarballFilterP = option readm $
  long "tarball-filter" <> short 'u' <> metavar "<tool>-<version>" <> value def
    <> help "Only check certain tarballs (format: <tool>-<version>)"
  where
    def = TarballFilter (Right Nothing) (makeRegex ("" :: String))
    readm = do
      s <- str
      case span (/= '-') s of
        (_, []) -> fail "invalid format, missing '-' after the tool name"
        (t, v) | [tool] <- [ tool | tool <- [minBound..maxBound], low (show tool) == low t ] ->
          pure (TarballFilter $ Right $ Just tool) <*> makeRegexOptsM compIgnoreCase execBlank (drop 1 v)
        (t, v) | [tool] <- [ tool | tool <- [minBound..maxBound], low (show tool) == low t ] ->
          pure (TarballFilter $ Left tool) <*> makeRegexOptsM compIgnoreCase execBlank (drop 1 v)
        _ -> fail "invalid tool"
    low = fmap toLower


opts :: Parser Options
opts = Options <$> com

com :: Parser Command
com = subparser
  (  command
       "check"
       (   ValidateYAML
       <$> info (validateYAMLOpts <**> helper)
                (progDesc "Validate the YAML")
       )
  <> command
       "check-tarballs"
       (info
         ((ValidateTarballs <$> validateYAMLOpts <*> tarballFilterP) <**> helper)
         (progDesc "Validate all tarballs (download and checksum)")
       )
  <> command
       "generate-hls-ghcs"
       (info
         ((GenerateHlsGhc <$> validateYAMLOpts <*> formatParser <*> outputP) <**> helper)
         (progDesc "Generate a list of HLS-GHC support")
       )
  <> command
       "generate-tool-table"
       (info
         ((GenerateToolTable <$> validateYAMLOpts <*> outputP) <**> helper)
         (progDesc "Generate a markdown table of available tool versions")
       )
  <> command
       "generate-system-deps-info"
       (info
         ((GenerateSystemDepsInfo <$> validateYAMLOpts <*> outputP) <**> helper)
         (progDesc "Generate a markdown info for system dependencies")
       )
  )



main :: IO ()
main = do
  no_color <- isJust <$> lookupEnv "NO_COLOR"
  let loggerConfig = LoggerConfig { lcPrintDebug  = True
                                  , consoleOutter = T.hPutStr stderr
                                  , fileOutter    = \_ -> pure ()
                                  , fancyColors   = not no_color
                                  }
  dirs <- liftIO getAllDirs
  let leanAppstate = LeanAppState (Settings True 0 False Never Curl True GHCupURL False GPGNone True) dirs defaultKeyBindings loggerConfig

  pfreq <- (
    flip runReaderT leanAppstate . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] $ platformRequest
    ) >>= \case
            VRight r -> pure r
            VLeft e -> do
              flip runReaderT leanAppstate $ logError $ T.pack $ prettyShow e
              liftIO $ exitWith (ExitFailure 2)

  let appstate = AppState (Settings True 0 False Never Curl True GHCupURL False GPGNone True) dirs defaultKeyBindings (GHCupInfo mempty mempty mempty) pfreq loggerConfig

  let withValidateYamlOpts vopts f = case vopts of
        ValidateYAMLOpts { vInput = Nothing } ->
          B.getContents >>= valAndExit f
        ValidateYAMLOpts { vInput = Just StdInput } ->
          B.getContents >>= valAndExit f
        ValidateYAMLOpts { vInput = Just (FileInput file) } ->
          B.readFile file >>= valAndExit f
      valAndExit f contents = do
        ginfo <- case Y.decodeEither' contents of
          Right r -> pure r
          Left  e -> die (color Red $ displayException e)
        r <- flip runReaderT appstate { ghcupInfo = ginfo } f
        exitWith r

  _ <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
    >>= \Options {..} -> case optCommand of
          ValidateYAML vopts -> withValidateYamlOpts vopts validate
          ValidateTarballs vopts tarballFilter -> withValidateYamlOpts vopts (validateTarballs tarballFilter)
          GenerateHlsGhc vopts format output -> withValidateYamlOpts vopts (generateHLSGhc format output)
          GenerateToolTable vopts output -> withValidateYamlOpts vopts (generateTable output)
          GenerateSystemDepsInfo vopts output -> withValidateYamlOpts vopts (generateSystemInfo output)
  pure ()

 where
