{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GHCup.OptParse.Test where




import GHCup.OptParse.Common

import GHCup.Command.Test.GHC
import GHCup.Errors
import GHCup.Input.Parsers     ( resolveVersion, uriParser )
import GHCup.Prelude
import GHCup.Prelude.String.QQ
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.Types
import GHCup.Types.Optics

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Options.Applicative             hiding ( ParseError, style )
import Options.Applicative.Pretty.Shim ( text )
import Prelude                         hiding ( appendFile )
import System.Exit
import URI.ByteString                  hiding ( uriParser )

import qualified Data.Text as T




    ----------------
    --[ Commands ]--
    ----------------


data TestCommand = TestGHC TestOptions




    ---------------
    --[ Options ]--
    ---------------


data TestOptions = TestOptions
  { testVer :: Maybe ToolVersion
  , testBindist :: Maybe URI
  , addMakeArgs :: [T.Text]
  }



    ---------------
    --[ Footers ]--
    ---------------

testFooter :: String
testFooter = [s|Discussion:
  Runs test suites from the test bindist.|]




    ---------------
    --[ Parsers ]--
    ---------------

testParser :: Parser TestCommand
testParser =
  subparser
      (  command
          "ghc"
          (   TestGHC
          <$> info
                (testOpts (Just ghc) <**> helper)
                (  progDesc "Test GHC"
                <> footerDoc (Just $ text testGHCFooter)
                )
          )
        )
 where
  testGHCFooter :: String
  testGHCFooter = [s|Discussion:
  Runs the GHC test suite from the test bindist.|]


testOpts :: Maybe Tool -> Parser TestOptions
testOpts tool =
  (\(u, v) args -> TestOptions v u args)
    <$> (   (   (,)
            <$> optional
                  (option
                    (eitherReader uriParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                      <> completer (toolDlCompleter (fromMaybe ghc tool))
                    )
                  )
            <*> (Just <$> toolVersionTagArgument [] tool)
            )
        <|> pure (Nothing, Nothing)
        )
    <*> many (argument str (metavar "MAKE_ARGS" <> help "Additional arguments to 'make', prefix with '-- ' (longopts)"))







    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type TestGHCEffects = [ DigestError
                      , ContentLengthError
                      , GPGError
                      , DownloadFailed
                      , NoDownload
                      , ArchiveResult
                      , TarDirDoesNotExist
                      , UnknownArchive
                      , TestFailed
                      , NextVerNotFound
                      , TagNotFound
                      , DayNotFound
                      , NoToolVersionSet
                      , URIParseError
                      , NoInstallInfo
                      , ParseError
                      ]



    -------------------
    --[ Entrypoints ]--
    -------------------


test :: TestCommand -> Settings -> (IO (AppState, IO ()), LeanAppState) -> IO ExitCode
test testCommand settings (getAppState', leanAppstate) = case testCommand of
  (TestGHC iopts) -> go iopts
 where
  guessMode = if guessVersion settings then GLaxWithInstalled else GStrict
  runLogger = flip runReaderT leanAppstate

  go :: TestOptions -> IO ExitCode
  go TestOptions{..} = do
    (s'@AppState{ dirs = Dirs{ .. } }, up) <- getAppState'
    let run appstate' = flip runReaderT (appstate' :: AppState)
            . runResourceT
            . runE
            @TestGHCEffects
    (case testBindist of
       Nothing -> run s' $ do
         GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
         treq@(TargetVersionReq tver _) <- liftE $ resolveVersion testVer guessMode ghc
         let vm = getVersionMetadata tver ghcup dls
         liftE $ testGHCVer treq addMakeArgs
         pure vm
       Just uri -> do
         run s'{ settings = settings {noVerify = True}} $ do
           GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
           (TargetVersionReq tver _) <- liftE $ resolveVersion testVer guessMode ghc
           let vm = getVersionMetadata tver ghcup dls
           liftE $ testGHCBindist (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) (Just $ RegexDir ".*/.*") "" Nothing Nothing Nothing Nothing) tver addMakeArgs
           pure vm
      )
        >>= \case
              VRight _ -> do
                runLogger $ logInfo "GHC test successful"
                liftIO up
                pure ExitSuccess
              VLeft e -> do
                runLogger $ do
                  logError $ T.pack $ prettyHFError e
                  logError $ "Also check the logs in " <> T.pack (fromGHCupPath logsDir)
                pure $ ExitFailure 3

