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
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Logger

#if !MIN_VERSION_base(4,13,0)
import           Data.Semigroup                 ( (<>) )
#endif
import           Options.Applicative     hiding ( style )
import           System.Console.Pretty
import           System.Exit
import           System.IO                      ( stdout )
import           Text.Regex.Posix
import           Validate

import qualified Data.ByteString               as B
import qualified Data.Yaml                     as Y


data Options = Options
  { optCommand :: Command
  }

data Command = ValidateYAML ValidateYAMLOpts
             | ValidateTarballs ValidateYAMLOpts (Maybe Regex)


data Input
  = FileInput FilePath -- optsparse-applicative doesn't handle ByteString correctly anyway
  | StdInput

fileInput :: Parser Input
fileInput =
  FileInput
    <$> (strOption
          (long "file" <> short 'f' <> metavar "FILENAME" <> help
            "Input file to validate"
          )
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

urlRegexP :: Parser (Maybe Regex)
urlRegexP = optional . option (str @String >>= makeRegexM) $
  long "url-regex" <> short 'u' <> metavar "URL_REGEX"
    <> help "Only validate if URL matches the regex"

opts :: Parser Options
opts = Options <$> com

com :: Parser Command
com = subparser
  (  (command
       "check"
       (   ValidateYAML
       <$> (info (validateYAMLOpts <**> helper)
                 (progDesc "Validate the YAML")
           )
       )
     )
  <> (command
       "check-tarballs"
       (info
         ((ValidateTarballs <$> validateYAMLOpts <*> urlRegexP) <**> helper)
         (progDesc "Validate all tarballs (download and checksum)")
       )
     )
  )



main :: IO ()
main = do
  _ <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
    >>= \Options {..} -> case optCommand of
          ValidateYAML vopts -> case vopts of
            ValidateYAMLOpts { vInput = Nothing } ->
              B.getContents >>= valAndExit validate
            ValidateYAMLOpts { vInput = Just StdInput } ->
              B.getContents >>= valAndExit validate
            ValidateYAMLOpts { vInput = Just (FileInput file) } ->
              B.readFile file >>= valAndExit validate
          ValidateTarballs vopts urlRegex -> case vopts of
            ValidateYAMLOpts { vInput = Nothing } ->
              B.getContents >>= valAndExit (validateTarballs urlRegex)
            ValidateYAMLOpts { vInput = Just StdInput } ->
              B.getContents >>= valAndExit (validateTarballs urlRegex)
            ValidateYAMLOpts { vInput = Just (FileInput file) } ->
              B.readFile file >>= valAndExit (validateTarballs urlRegex)
  pure ()

 where
  valAndExit f contents = do
    (GHCupInfo _ av) <- case Y.decodeEither' contents of
      Right r -> pure r
      Left  e -> die (color Red $ show e)
    myLoggerT (LoggerConfig True (B.hPut stdout) (\_ -> pure ())) (f av)
      >>= exitWith
