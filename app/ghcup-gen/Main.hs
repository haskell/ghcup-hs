{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Logger
import           GHCupInfo

import           Data.Aeson                     ( eitherDecode )
import           Data.Aeson.Encode.Pretty
#if !MIN_VERSION_base(4,13,0)
import           Data.Semigroup                 ( (<>) )
#endif
import           Options.Applicative     hiding ( style )
import           System.Console.Pretty
import           System.Exit
import           System.IO                      ( stdout )
import           Validate

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L


data Options = Options
  { optCommand :: Command
  }

data Command = GenJSON GenJSONOpts
             | ValidateJSON ValidateJSONOpts
             | ValidateTarballs ValidateJSONOpts

data Output
  = FileOutput FilePath -- optsparse-applicative doesn't handle ByteString correctly anyway
  | StdOutput

fileOutput :: Parser Output
fileOutput =
  FileOutput
    <$> (strOption
          (long "file" <> short 'f' <> metavar "FILENAME" <> help
            "Output to a file"
          )
        )

stdOutput :: Parser Output
stdOutput = flag'
  StdOutput
  (short 'o' <> long "stdout" <> help "Print to stdout (default)")

outputP :: Parser Output
outputP = fileOutput <|> stdOutput


data GenJSONOpts = GenJSONOpts
  { output :: Maybe Output
  }

genJSONOpts :: Parser GenJSONOpts
genJSONOpts = GenJSONOpts <$> optional outputP


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

data ValidateJSONOpts = ValidateJSONOpts
  { input :: Maybe Input
  }

validateJSONOpts :: Parser ValidateJSONOpts
validateJSONOpts = ValidateJSONOpts <$> optional inputP

opts :: Parser Options
opts = Options <$> com

com :: Parser Command
com = subparser
  (  (command
       "gen"
       (   GenJSON
       <$> (info (genJSONOpts <**> helper)
                 (progDesc "Generate the json downloads file")
           )
       )
     )
  <> (command
       "check"
       (   ValidateJSON
       <$> (info (validateJSONOpts <**> helper)
                 (progDesc "Validate the JSON")
           )
       )
     )
  <> (command
       "check-tarballs"
       (   ValidateTarballs
       <$> (info
             (validateJSONOpts <**> helper)
             (progDesc "Validate all tarballs (download and checksum)")
           )
       )
     )
  )



main :: IO ()
main = do
  customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
    >>= \Options {..} -> case optCommand of
          GenJSON gopts -> do
            let
              bs = encodePretty' (defConfig { confIndent = Spaces 2 })
                                 ghcupInfo
            case gopts of
              GenJSONOpts { output = Nothing }        -> L.hPutStr stdout bs
              GenJSONOpts { output = Just StdOutput } -> L.hPutStr stdout bs
              GenJSONOpts { output = Just (FileOutput file) } ->
                L.writeFile file bs
          ValidateJSON vopts -> case vopts of
            ValidateJSONOpts { input = Nothing } ->
              L.getContents >>= valAndExit validate
            ValidateJSONOpts { input = Just StdInput } ->
              L.getContents >>= valAndExit validate
            ValidateJSONOpts { input = Just (FileInput file) } ->
              L.readFile file >>= valAndExit validate
          ValidateTarballs vopts -> case vopts of
            ValidateJSONOpts { input = Nothing } ->
              L.getContents >>= valAndExit validateTarballs
            ValidateJSONOpts { input = Just StdInput } ->
              L.getContents >>= valAndExit validateTarballs
            ValidateJSONOpts { input = Just (FileInput file) } ->
              L.readFile file >>= valAndExit validateTarballs
  pure ()

 where
  valAndExit f contents = do
    (GHCupInfo _ av) <- case eitherDecode contents of
      Right r -> pure r
      Left  e -> die (color Red $ show e)
    myLoggerT (LoggerConfig True (B.hPut stdout) (\_ -> pure ())) (f av)
      >>= exitWith

