{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeOperators     #-}

module GHCup.OptParse.Test where




import           GHCup.OptParse.Common

import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils.Dirs
import           GHCup.Utils.Parsers (fromVersion, uriParser)
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           URI.ByteString          hiding ( uriParser )

import qualified Data.Text                     as T




    ----------------
    --[ Commands ]--
    ----------------


data TestCommand = TestGHC TestOptions




    ---------------
    --[ Options ]--
    ---------------


data TestOptions = TestOptions
  { testVer      :: Maybe ToolVersion
  , testBindist  :: Maybe URI
  , addMakeArgs  :: [T.Text]
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
                (testOpts (Just GHC) <**> helper)
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
                      <> completer (toolDlCompleter (fromMaybe GHC tool))
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
                      ]

runTestGHC :: AppState
           -> Excepts TestGHCEffects (ResourceT (ReaderT AppState IO)) a
           -> IO (VEither TestGHCEffects a)
runTestGHC appstate' =
  flip runReaderT appstate'
  . runResourceT
  . runE
    @TestGHCEffects


    -------------------
    --[ Entrypoints ]--
    -------------------


test :: TestCommand -> Settings -> IO AppState -> (ReaderT LeanAppState IO () -> IO ()) -> IO ExitCode
test testCommand settings getAppState' runLogger = case testCommand of
  (TestGHC iopts) -> go iopts
 where
  go :: TestOptions -> IO ExitCode
  go TestOptions{..} = do
    s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
    (case testBindist of
       Nothing -> runTestGHC s' $ do
         (v, vi) <- liftE $ fromVersion testVer GHC
         liftE $ testGHCVer v addMakeArgs
         pure vi
       Just uri -> do
         runTestGHC s'{ settings = settings {noVerify = True}} $ do
           (v, vi) <- liftE $ fromVersion testVer GHC
           liftE $ testGHCBindist (DownloadInfo uri (Just $ RegexDir ".*/.*") "" Nothing Nothing) v addMakeArgs
           pure vi
      )
        >>= \case
              VRight _ -> do
                runLogger $ logInfo "GHC test successful"
                pure ExitSuccess
              VLeft e -> do
                runLogger $ do
                  logError $ T.pack $ prettyHFError e
                  logError $ "Also check the logs in " <> T.pack (fromGHCupPath logsDir)
                pure $ ExitFailure 3

