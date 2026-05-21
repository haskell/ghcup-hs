{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Generate where


#if defined(DHALL)
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Input.Parsers
import           GHCup.OptParse.Common
import           GHCup.Types.Dhall ()
import           GHCup.Prelude.Logger
import GHCup.Types.Optics
import GHCup.Download
import GHCup.Prelude (decUTF8Safe)

import           Control.Monad.Reader
import           Data.Functor
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style, ParseError )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import Dhall.Marshal.Encode ()

import qualified Dhall.Core as Dhall
import qualified Dhall.Binary as Dhall
import qualified Data.Either.Validation as Validation
import qualified Dhall hiding (Text)

import qualified Data.Text                     as T
import qualified Data.Text.IO                     as T
import qualified Data.Yaml as YAML
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Void as V





    ---------------
    --[ Options ]--
    ---------------

data GenerateCommand = GenerateDhallSchema   GenOutput
                     | GenerateDhallFromYaml ToDhallOptions
                     | GenerateYamlFromDhall GenInput GenOutput


data ToDhallOptions = ToDhallOptions {
    genInput  :: GenInput
  , genOutput :: GenOutput
  , genBinary :: Bool
  }

data GenOutput = GenOutputFile FilePath
               | GenStdOut


data GenInput = GenInputURL [NewURLSource]
              | GenStdIn


    ---------------
    --[ Parsers ]--
    ---------------

generateP :: Parser GenerateCommand
generateP =
  subparser
      (  command
          "dhall-schema"
          (GenerateDhallSchema <$> info (genOutputP <**> helper) (progDesc "Generate Dhall schema from the GHCup types"))
      <> command
          "metadata-to-dhall"
          (GenerateDhallFromYaml
              <$> info (toDhallOptions <**> helper)
            (progDesc "Convert the input metadata to binary Dhall")
          )
      <> command
          "metadata-to-yaml"
          ((\(in', out) -> GenerateYamlFromDhall in' out)
              <$> info ((,) <$> genInputP <*> genOutputP <**> helper)
            (progDesc "Convert the input metadata to expanded YAML")
          )
      )

toDhallOptions :: Parser ToDhallOptions
toDhallOptions =
  ToDhallOptions
    <$> genInputP
    <*> genOutputP
    <*> (
      switch
          (short 'b' <> long "binary" <> help "Convert to Dhall binary format")
    )

genInputP :: Parser GenInput
genInputP =
  maybe GenStdIn GenInputURL <$>
    optional parseUrlSourceP

genOutputP :: Parser GenOutput
genOutputP =
  maybe GenStdOut GenOutputFile <$>
    optional
      (option
       (eitherReader filePathParser)
       (  short 'o'
       <> long "output-file"
       <> metavar "FILEPATH"
       <> help "Write the output to the specified file instead of stdout"
       <> completer (bashCompleter "file")
       )
      )



    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type GenerateEffects = '[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, StackPlatformDetectError, UnsupportedMetadataFormat, ParseError]




    ------------------
    --[ Entrypoint ]--
    ------------------



generate ::
  ( MonadIOish m
  )
   => GenerateCommand
   -> (IO (AppState, IO ()), LeanAppState)
   -> m ExitCode
generate gen (_getAppState', leanAppstate) = runLean (do
  pfreq <- lift getPlatformReq
  case gen of
    GenerateDhallSchema genOutput -> do
       withValidation (Dhall.expected (Dhall.auto @GHCupInfo)) $ \(Dhall.pretty -> result) -> do
         toOutputT genOutput result
         pure ()
    GenerateDhallFromYaml (ToDhallOptions genInput genOutput b) -> do
      r <- getInput genInput pfreq
      let dhallExpr = Dhall.denote $ (Dhall.embed @GHCupInfo Dhall.inject) r
      if b
      then toOutputBSL genOutput $ Dhall.encodeExpression dhallExpr
      else toOutputT genOutput $ Dhall.pretty dhallExpr
      pure ()
    GenerateYamlFromDhall genInput genOutput -> do
      r <- getInput genInput pfreq
      toOutputBS genOutput $ YAML.encode r
   ) >>= \case
            VRight _ -> do
              pure ExitSuccess
            VLeft e -> do
              runLogger $ logError $ T.pack $ prettyHFError e
              pure $ ExitFailure 27
 where
  getInput ::
    ( MonadReader env m
    , HasSettings env
    , HasDirs env
    , HasLog env
    , MonadIOish m
    )
    => GenInput
    -> PlatformRequest
    -> Excepts GenerateEffects m GHCupInfo
  getInput genInput pfreq =
    case genInput of
      GenInputURL xs -> do
        ghcupInfo <- liftE $ getDownloadsF' pfreq xs
        pure ghcupInfo
      GenStdIn -> do
        input <- liftIO BS.getContents
        liftIO $ asum [yaml input, dhallB input, dhall input]
   where
    yaml :: BS.ByteString -> IO GHCupInfo
    yaml input = either (fail . show) pure $ YAML.decodeEither' input

    dhallB :: BS.ByteString -> IO GHCupInfo
    dhallB input = do
      expr <- either (fail . show) pure $ Dhall.decodeExpression @V.Void @V.Void (BSL.fromStrict input)
      Dhall.rawInput Dhall.auto expr

    dhall :: BS.ByteString -> IO GHCupInfo
    dhall (decUTF8Safe -> input) = liftIO $ Dhall.input Dhall.auto input

  withValidation val action' =
    case val of
      Validation.Success r -> action' r
      Validation.Failure errors -> do
        throwE $ ParseError (show errors)
  toOutputT genOutput out =
    case genOutput of
    GenStdOut ->
      liftIO $ T.putStrLn out
    GenOutputFile fp ->
      liftIO $ T.writeFile fp out
  toOutputBSL genOutput out =
    case genOutput of
    GenStdOut ->
      liftIO $ BSL.putStr out
    GenOutputFile fp ->
      liftIO $ BSL.writeFile fp out
  toOutputBS genOutput out =
    case genOutput of
    GenStdOut ->
      liftIO $ BS.putStr out
    GenOutputFile fp ->
      liftIO $ BS.writeFile fp out
  runLogger = flip runReaderT leanAppstate
  runLean = flip runReaderT leanAppstate . runE @GenerateEffects

#endif
