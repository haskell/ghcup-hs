{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module GHCup.OptParse.Run where


import           GHCup
import           GHCup.Utils
import           GHCup.Utils.Parsers (fromVersion, ghcVersionTagEither, isolateParser, toolVersionTagEither)
import           GHCup.OptParse.Common
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Prelude
import           GHCup.Prelude.File
#ifdef IS_WINDOWS
import           GHCup.Prelude.Process
import           GHCup.Prelude.Process.Windows ( execNoMinGW )
#endif
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ

import           Control.Exception.Safe         ( MonadMask, MonadCatch )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe (isNothing)
import           Data.List                      ( intercalate )
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.FilePath
import           System.Environment
import           System.Exit

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
#ifndef IS_WINDOWS
import qualified System.Posix.Process          as SPP
#endif
import Data.Versions ( prettyVer, Version )





    ---------------
    --[ Options ]--
    ---------------


data RunOptions = RunOptions
  { runAppendPATH :: Bool
  , runInstTool'  :: Bool
  , runMinGWPath  :: Bool
  , runGHCVer     :: Maybe ToolVersion
  , runCabalVer   :: Maybe ToolVersion
  , runHLSVer     :: Maybe ToolVersion
  , runStackVer   :: Maybe ToolVersion
  , runBinDir     :: Maybe FilePath
  , runQuick      :: Bool
  , runCOMMAND    :: [String]
  } deriving (Eq, Show)



    ---------------
    --[ Parsers ]--
    ---------------



runOpts :: Parser RunOptions
runOpts =
  RunOptions
    <$> switch
          (short 'a' <> long "append" <> help "Append bin/ dir to PATH instead of prepending (this means that e.g. a system installation may take precedence)")
    <*> switch
          (short 'i' <> long "install" <> help "Install the tool, if missing")
    <*> switch
          (short 'm' <> long "mingw-path" <> help "On windows, add mingw64 PATHs to environment (does nothing on unix)")
    <*> optional
          (option
            (eitherReader ghcVersionTagEither)
            (metavar "GHC_VERSION" <> long "ghc" <> help "The ghc version"
            <> completer (tagCompleter GHC [])
            <> (completer $ versionCompleter [] GHC)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "CABAL_VERSION" <> long "cabal" <> help "The cabal version"
            <> completer (tagCompleter Cabal [])
            <> (completer $ versionCompleter [] Cabal)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "HLS_VERSION" <> long "hls" <> help "The HLS version"
            <> completer (tagCompleter HLS [])
            <> (completer $ versionCompleter [] HLS)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "STACK_VERSION" <> long "stack" <> help "The stack version"
            <> completer (tagCompleter Stack [])
            <> (completer $ versionCompleter [] Stack)
            )
          )
    <*> optional
          (option
           (eitherReader isolateParser)
           (  short 'b'
           <> long "bindir"
           <> metavar "DIR"
           <> help "directory where to create the tool symlinks (default: newly created system temp dir)"
           <> completer (bashCompleter "directory")
           )
          )
    <*> switch
          (short 'q' <> long "quick" <> help "Avoid any expensive work (such as downloads, version/tag resolution etc.). Disables --install.")
    <*> many (argument str (metavar "COMMAND" <> help "The command to run, with arguments (use longopts --). If omitted, just prints the created bin/ dir to stdout and exits."))




    --------------
    --[ Footer ]--
    --------------


runFooter :: String
runFooter = [s|Discussion:
  Adds the given tools to a dedicated bin/ directory and adds them to PATH, exposing
  the relevant binaries, then executes a command.

Examples:
  # run VSCode with all latest toolchain exposed, installing missing versions if necessary
  ghcup run --ghc latest --cabal latest --hls latest --stack latest --install -- code Setup.hs

  # create a custom toolchain bin/ dir with GHC and cabal that can be manually added to PATH
  ghcup run --ghc 8.10.7 --cabal 3.2.0.0 --bindir $HOME/toolchain/bin

  # run a specific ghc version
  ghcup run --ghc 8.10.7 -- ghc --version|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type RunEffects = '[ AlreadyInstalled
                   , UnknownArchive
                   , ArchiveResult
                   , FileDoesNotExistError
                   , CopyError
                   , NotInstalled
                   , DirNotEmpty
                   , NoDownload
                   , NotInstalled
                   , BuildFailed
                   , TagNotFound
                   , DayNotFound
                   , DigestError
                   , ContentLengthError
                   , GPGError
                   , DownloadFailed
                   , TarDirDoesNotExist
                   , NextVerNotFound
                   , NoToolVersionSet
                   , FileAlreadyExistsError
                   , ProcessError
                   , UninstallFailed
                   , MergeFileTreeError
                   , NoCompatiblePlatform
                   , GHCup.Errors.ParseError
                   , UnsupportedSetupCombo
                   , DistroNotFound
                   , NoCompatibleArch
                   ]

runLeanRUN :: (MonadUnliftIO m, MonadIO m)
           => LeanAppState
           -> Excepts RunEffects (ReaderT LeanAppState m) a
           -> m (VEither RunEffects a)
runLeanRUN leanAppstate =
    -- Don't use runLeanAppState here, which is disabled on windows.
    -- This is the only command on all platforms that doesn't need full appstate.
    flip runReaderT leanAppstate
    . runE
      @RunEffects

runRUN :: MonadUnliftIO m
      => IO AppState
      -> Excepts RunEffects (ResourceT (ReaderT AppState m)) a
      -> m (VEither RunEffects a)
runRUN appState action' = do
  s' <- liftIO appState
  flip runReaderT s'
    . runResourceT
    . runE
      @RunEffects
    $ action'



    ------------------
    --[ Entrypoint ]--
    ------------------



run :: forall m .
       ( MonadFail m
       , MonadMask m
       , MonadCatch m
       , MonadIO m
       , MonadUnliftIO m
       , Alternative m
       )
   => RunOptions
   -> IO AppState
   -> LeanAppState
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
run RunOptions{..} runAppState leanAppstate runLogger = do
   r <- if not runQuick
        then runRUN runAppState $ do
         toolchain <- liftE resolveToolchainFull

         -- oh dear
         r <- lift ask
         tmp <- lift . lift . lift . flip runReaderT (fromAppState r) $ createTmpDir toolchain

         liftE $ installToolChainFull toolchain tmp
         pure tmp
        else runLeanRUN leanAppstate $ do
         toolchain <- resolveToolchain
         tmp <- lift $ createTmpDir toolchain
         liftE $ installToolChain toolchain tmp
         pure tmp
   case r of
         VRight tmp -> do
           case runCOMMAND of
             [] -> do
               liftIO $ putStr tmp
               pure ExitSuccess
             (cmd:args) -> do
               newEnv <- liftIO $ addToPath [tmp] runAppendPATH
               let pathVar = if isWindows then "Path" else "PATH"
               forM_ (Map.lookup pathVar . Map.fromList $ newEnv) $ liftIO . setEnv pathVar
#ifndef IS_WINDOWS
               void $ liftIO $ SPP.executeFile cmd True args (Just newEnv)
               pure ExitSuccess
#else
               r' <- if runMinGWPath
                     then runLeanRUN leanAppstate $ liftE $ lEM @_ @'[ProcessError] $ exec cmd args Nothing (Just newEnv)
                     else runLeanRUN leanAppstate $ liftE $ lEM @_ @'[ProcessError] $ execNoMinGW cmd args Nothing (Just newEnv)
               case r' of
                 VRight _ -> pure ExitSuccess
                 VLeft e -> do
                   runLogger $ logError $ T.pack $ prettyHFError e
                   pure $ ExitFailure 28
#endif
         VLeft e -> do
           runLogger $ logError $ T.pack $ prettyHFError e
           pure $ ExitFailure 27

  where

   -- TODO: doesn't work for cross
   resolveToolchainFull :: ( MonadFail m
                           , MonadThrow m
                           , MonadIO m
                           , MonadCatch m
                           )
                        => Excepts
                             '[ TagNotFound
                              , DayNotFound
                              , NextVerNotFound
                              , NoToolVersionSet
                              ] (ResourceT (ReaderT AppState m)) Toolchain
   resolveToolchainFull = do
         ghcVer <- forM runGHCVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) GHC
           pure v
         cabalVer <- forM runCabalVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) Cabal
           pure (_tvVersion v)
         hlsVer <- forM runHLSVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) HLS
           pure (_tvVersion v)
         stackVer <- forM runStackVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) Stack
           pure (_tvVersion v)
         pure Toolchain{..}

   resolveToolchain = do
         ghcVer <- case runGHCVer of
            Just (GHCVersion v) -> pure $ Just v
            Just (ToolVersion v) -> pure $ Just (mkTVer v)
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         cabalVer <- case runCabalVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         hlsVer <- case runHLSVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         stackVer <- case runStackVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         pure Toolchain{..}

   installToolChainFull :: ( MonadFail m
                           , MonadThrow m
                           , MonadIO m
                           , MonadCatch m
                           , Alternative m
                           )
                        => Toolchain
                        -> FilePath
                        -> Excepts
                             '[ TagNotFound
                              , DayNotFound
                              , NextVerNotFound
                              , NoToolVersionSet
                              , UnknownArchive
                              , TarDirDoesNotExist
                              , ProcessError
                              , NotInstalled
                              , NoDownload
                              , GPGError
                              , DownloadFailed
                              , DirNotEmpty
                              , DigestError
                              , ContentLengthError
                              , BuildFailed
                              , ArchiveResult
                              , AlreadyInstalled
                              , FileAlreadyExistsError
                              , CopyError
                              , UninstallFailed
                              , MergeFileTreeError
                              , NoCompatiblePlatform
                              , GHCup.Errors.ParseError
                              , UnsupportedSetupCombo
                              , DistroNotFound
                              , NoCompatibleArch
                              ] (ResourceT (ReaderT AppState m)) ()
   installToolChainFull Toolchain{..} tmp = do
         case ghcVer of
           Just v -> do
             isInstalled <- lift $ checkIfToolInstalled' GHC v
             unless isInstalled $ when (runInstTool' && isNothing (_tvTarget v)) $ void $ liftE $ installGHCBin
               v
               GHCupInternal
               False
               []
             setGHC' v tmp
           _ -> pure ()
         case cabalVer of
           Just v -> do
             isInstalled <- lift $ checkIfToolInstalled' Cabal (mkTVer v)
             unless isInstalled $ when runInstTool' $ void $ liftE $ installCabalBin
               v
               GHCupInternal
               False
             setCabal' v tmp
           _ -> pure ()
         case stackVer of
           Just v -> do
             isInstalled <- lift $ checkIfToolInstalled' Stack (mkTVer v)
             unless isInstalled $ when runInstTool' $ void $ liftE $ installStackBin
               v
               GHCupInternal
               False
             setStack' v tmp
           _ -> pure ()
         case hlsVer of
           Just v -> do
             isInstalled <- lift $ checkIfToolInstalled' HLS (mkTVer v)
             unless isInstalled $ when runInstTool' $ void $ liftE $ installHLSBin
               v
               GHCupInternal
               False
             setHLS' v tmp
           _ -> pure ()

   installToolChain :: ( MonadFail m
                       , MonadThrow m
                       , MonadIO m
                       , MonadCatch m
                       )
                    => Toolchain
                    -> FilePath
                    -> Excepts '[NotInstalled] (ReaderT LeanAppState m) ()
   installToolChain Toolchain{..} tmp = do
         case ghcVer of
           Just v -> setGHC' v tmp
           _ -> pure ()
         case cabalVer of
           Just v -> setCabal' v tmp
           _ -> pure ()
         case stackVer of
           Just v -> setStack' v tmp
           _ -> pure ()
         case hlsVer of
           Just v -> setHLS' v tmp
           _ -> pure ()

   setGHC' v tmp = do
          void $ liftE $ setGHC v SetGHC_XYZ (Just tmp)
          void $ liftE $ setGHC v SetGHCOnly (Just tmp)
   setCabal' v tmp = do
          bin  <- liftE $ whereIsTool Cabal (mkTVer v)
          cbin <- liftIO $ canonicalizePath bin
          lift $ createLink (relativeSymlink tmp cbin) (tmp </> ("cabal" <.> exeExt))
   setStack' v tmp = do
          bin  <- liftE $ whereIsTool Stack (mkTVer v)
          cbin <- liftIO $ canonicalizePath bin
          lift $ createLink (relativeSymlink tmp cbin) (tmp </> ("stack" <.> exeExt))
   setHLS' v tmp = do
          Dirs {..}  <- getDirs
          legacy <- isLegacyHLS v
          if legacy
          then do
            -- TODO: factor this out
            hlsWrapper <- liftE @_ @'[NotInstalled] $ hlsWrapperBinary v !? (NotInstalled HLS (mkTVer v))
            cw <- liftIO $ canonicalizePath (binDir </> hlsWrapper)
            lift $ createLink (relativeSymlink tmp cw) (tmp </> takeFileName cw)
            hlsBins <- hlsServerBinaries v Nothing >>= liftIO . traverse (canonicalizePath . (binDir </>))
            forM_ hlsBins $ \bin ->
              lift $ createLink (relativeSymlink tmp bin) (tmp </> takeFileName bin)
            liftE $ setHLS v SetHLSOnly (Just tmp)
          else do
            liftE $ setHLS v SetHLS_XYZ (Just tmp)
            liftE $ setHLS v SetHLSOnly (Just tmp)

   createTmpDir :: ( MonadUnliftIO m
                   , MonadCatch m
                   , MonadThrow m
                   , MonadMask m
                   , MonadIO m
                   )
                => Toolchain
                -> ReaderT LeanAppState m FilePath
   createTmpDir toolchain =
     case runBinDir of
           Just bindir -> do
             liftIO $ createDirRecursive' bindir
             liftIO $ canonicalizePath bindir
           Nothing -> do
             d <- predictableTmpDir toolchain
             liftIO $ createDirRecursive' d
             liftIO $ canonicalizePath d

   predictableTmpDir :: Monad m
                     => Toolchain
                     -> ReaderT LeanAppState m FilePath
   predictableTmpDir (Toolchain Nothing Nothing Nothing Nothing) = do
     Dirs { tmpDir } <- getDirs
     pure (fromGHCupPath tmpDir </> "ghcup-none")
   predictableTmpDir Toolchain{..} = do
      Dirs { tmpDir } <- getDirs
      pure $ fromGHCupPath tmpDir
        </> ("ghcup-" <> intercalate "_"
              (  maybe [] ( (:[]) . ("ghc-"   <>) . T.unpack . tVerToText) ghcVer
              <> maybe [] ( (:[]) . ("cabal-" <>) . T.unpack . prettyVer) cabalVer
              <> maybe [] ( (:[]) . ("hls-"   <>) . T.unpack . prettyVer) hlsVer
              <> maybe [] ( (:[]) . ("stack-" <>) . T.unpack . prettyVer) stackVer
              )
            )



    -------------------------
    --[ Other local types ]--
    -------------------------



data Toolchain = Toolchain
  { ghcVer     :: Maybe GHCTargetVersion
  , cabalVer   :: Maybe Version
  , hlsVer     :: Maybe Version
  , stackVer   :: Maybe Version
  } deriving Show
