{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}


module Main where

import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Utils.Dirs
import           GHCup.Types.JSON               ( )

import           Control.Monad.Trans.Reader     ( runReaderT )
import           Control.Monad.IO.Class
import           Data.Char                      ( toLower )
#if !MIN_VERSION_base(4,13,0)
import           Data.Semigroup                 ( (<>) )
#endif
import           Options.Applicative     hiding ( style )
import           Haskus.Utils.Variant.Excepts
import           System.Console.Pretty
import           System.Exit
import           System.IO                      ( stderr )
import           Text.Regex.Posix
import           Validate
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Text.IO                  as T
import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import qualified Data.YAML.Aeson               as Y


data Options = Options
  { optCommand :: Command
  }

data Command = ValidateYAML ValidateYAMLOpts
             | ValidateTarballs ValidateYAMLOpts TarballFilter


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
  )



main :: IO ()
main = do
  let loggerConfig = LoggerConfig { lcPrintDebug = True
                                  , colorOutter  = T.hPutStr stderr
                                  , rawOutter    = \_ -> pure ()
                                  }
  dirs <- liftIO getAllDirs
  let leanAppstate = LeanAppState (Settings True False Never Curl True GHCupURL False) dirs defaultKeyBindings loggerConfig

  pfreq <- (
    flip runReaderT leanAppstate . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] $ platformRequest
    ) >>= \case
            VRight r -> pure r
            VLeft e -> do
              flip runReaderT leanAppstate $ logError $ T.pack $ prettyShow e
              liftIO $ exitWith (ExitFailure 2)

  let appstate = AppState (Settings True False Never Curl True GHCupURL False) dirs defaultKeyBindings (GHCupInfo mempty mempty mempty) pfreq loggerConfig

  _ <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
    >>= \Options {..} -> case optCommand of
          ValidateYAML vopts -> withValidateYamlOpts vopts (\dl m -> flip runReaderT appstate $ validate dl m)
          ValidateTarballs vopts tarballFilter -> withValidateYamlOpts vopts (\dl m -> flip runReaderT appstate $ validateTarballs tarballFilter dl m)
  pure ()

 where
  withValidateYamlOpts vopts f = case vopts of
    ValidateYAMLOpts { vInput = Nothing } ->
      B.getContents >>= valAndExit f
    ValidateYAMLOpts { vInput = Just StdInput } ->
      B.getContents >>= valAndExit f
    ValidateYAMLOpts { vInput = Just (FileInput file) } ->
      B.readFile file >>= valAndExit f
  valAndExit f contents = do
    (GHCupInfo _ av gt) <- case Y.decode1Strict contents of
      Right r -> pure r
      Left  (_, e) -> die (color Red $ show e)
    f av gt
      >>= exitWith
