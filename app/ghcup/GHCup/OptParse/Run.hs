{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
module GHCup.OptParse.Run where


import           GHCup
import           GHCup.Utils
import           GHCup.Utils.Prelude
import           GHCup.Utils.File
import           GHCup.OptParse.Common
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils.Logger
import           GHCup.Utils.String.QQ

import           Control.Exception.Safe         ( MonadMask, MonadCatch )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Codec.Archive
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe (isNothing)
import           Data.List                      ( intercalate )
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.Exit
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
#ifndef IS_WINDOWS
import qualified System.Posix.Process          as SPP
#endif





    ---------------
    --[ Options ]--
    ---------------


data RunOptions = RunOptions
  { runAppendPATH :: Bool
  , runInstTool'  :: Bool
  , runGHCVer     :: Maybe ToolVersion
  , runCabalVer   :: Maybe ToolVersion
  , runHLSVer     :: Maybe ToolVersion
  , runStackVer   :: Maybe ToolVersion
  , runBinDir     :: Maybe FilePath
  , runQuick      :: Bool
  , runCOMMAND    :: [String]
  }



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
    <*> optional
          (option
            (eitherReader toolVersionEither)
            (metavar "GHC_VERSION" <> long "ghc" <> help "The ghc version"
            <> completer (tagCompleter GHC [])
            <> (completer $ versionCompleter Nothing GHC)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionEither)
            (metavar "CABAL_VERSION" <> long "cabal" <> help "The cabal version"
            <> completer (tagCompleter Cabal [])
            <> (completer $ versionCompleter Nothing Cabal)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionEither)
            (metavar "HLS_VERSION" <> long "hls" <> help "The HLS version"
            <> completer (tagCompleter HLS [])
            <> (completer $ versionCompleter Nothing HLS)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionEither)
            (metavar "STACK_VERSION" <> long "stack" <> help "The stack version"
            <> completer (tagCompleter Stack [])
            <> (completer $ versionCompleter Nothing Stack)
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
                   , DigestError
                   , GPGError
                   , DownloadFailed
                   , TarDirDoesNotExist
                   , NextVerNotFound
                   , NoToolVersionSet
                   , FileAlreadyExistsError
                   , ProcessError
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



run :: forall m. 
       ( MonadFail m
       , MonadMask m
       , MonadCatch m
       , MonadIO m
       , MonadUnliftIO m
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
         tmp <- liftIO $ createTmpDir toolchain
         liftE $ installToolChainFull toolchain tmp
         pure tmp
        else runLeanRUN leanAppstate $ do
         toolchain <- resolveToolchain
         tmp <- liftIO $ createTmpDir toolchain
         liftE $ installToolChain toolchain tmp
         pure tmp
   case r of
         VRight tmp -> do
           case runCOMMAND of
             [] -> do
               liftIO $ putStr tmp
               pure ExitSuccess
             (cmd:args) -> do
               newEnv <- liftIO $ addToPath tmp
#ifndef IS_WINDOWS
               void $ liftIO $ SPP.executeFile cmd True args (Just newEnv)
               pure ExitSuccess
#else
               r' <- runLeanRUN leanAppstate $ liftE $ lEM @_ @'[ProcessError] $ exec cmd args Nothing (Just newEnv)
               case r' of
                 VRight _ -> pure ExitSuccess
                 VLeft e -> do
                   runLogger $ logError $ T.pack $ prettyShow e
                   pure $ ExitFailure 28
#endif
         VLeft e -> do
           runLogger $ logError $ T.pack $ prettyShow e
           pure $ ExitFailure 27

  where

   createTmpDir :: Toolchain -> IO FilePath
   createTmpDir toolchain =
     case runBinDir of
           Just bindir -> do
             createDirRecursive' bindir
             canonicalizePath bindir
           Nothing -> do
             d <- predictableTmpDir toolchain
             createDirRecursive' d
             canonicalizePath d

   -- TODO: doesn't work for cross
   resolveToolchainFull :: ( MonadFail m
                           , MonadThrow m
                           , MonadIO m
                           , MonadCatch m
                           )
                        => Excepts
                             '[ TagNotFound
                              , NextVerNotFound
                              , NoToolVersionSet
                              ] (ResourceT (ReaderT AppState m)) Toolchain
   resolveToolchainFull = do
         ghcVer <- forM runGHCVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) GHC
           pure v
         cabalVer <- forM runCabalVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) Cabal
           pure v
         hlsVer <- forM runHLSVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) HLS
           pure v
         stackVer <- forM runStackVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) Stack
           pure v
         pure Toolchain{..}

   resolveToolchain = do
         ghcVer <- case runGHCVer of
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         cabalVer <- case runCabalVer of
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         hlsVer <- case runHLSVer of
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         stackVer <- case runStackVer of
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> fail "Internal error"
         pure Toolchain{..}

   installToolChainFull :: ( MonadFail m
                           , MonadThrow m
                           , MonadIO m
                           , MonadCatch m
                           )
                        => Toolchain
                        -> FilePath
                        -> Excepts
                             '[ TagNotFound
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
                              , BuildFailed
                              , ArchiveResult
                              , AlreadyInstalled
                              , FileAlreadyExistsError
                              , CopyError
                              ] (ResourceT (ReaderT AppState m)) ()
   installToolChainFull Toolchain{..} tmp = do
         forM_ [(GHC,) <$> ghcVer, (Cabal,) <$> cabalVer, (HLS,) <$> hlsVer, (Stack,) <$> stackVer] $ \mt -> do
           isInstalled <- maybe (pure False) (\(tool, v) -> lift $ checkIfToolInstalled' tool v) mt
           case mt of
             Just (GHC, v) -> do
               unless isInstalled $ when (runInstTool' && isNothing (_tvTarget v)) $ void $ liftE $ installGHCBin
                 (_tvVersion v)
                 GHCupInternal
                 False
               setTool GHC v tmp
             Just (Cabal, v) -> do
               unless isInstalled $ when runInstTool' $ void $ liftE $ installCabalBin
                 (_tvVersion v)
                 GHCupInternal
                 False
               setTool Cabal v tmp
             Just (Stack, v) -> do
               unless isInstalled $ when runInstTool' $ void $ liftE $ installStackBin
                 (_tvVersion v)
                 GHCupInternal
                 False
               setTool Stack v tmp
             Just (HLS, v) -> do
               unless isInstalled $ when runInstTool' $ void $ liftE $ installHLSBin
                 (_tvVersion v)
                 GHCupInternal
                 False
               setTool HLS v tmp
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
         forM_ [(GHC,) <$> ghcVer, (Cabal,) <$> cabalVer, (HLS,) <$> hlsVer, (Stack,) <$> stackVer] $ \mt -> do
           case mt of
             Just (GHC, v)   -> setTool GHC v tmp
             Just (Cabal, v) -> setTool Cabal v tmp
             Just (Stack, v) -> setTool Stack v tmp
             Just (HLS, v)   -> setTool HLS v tmp
             _ -> pure ()

   setTool tool v tmp =
      case tool of
        GHC -> do
          void $ liftE $ setGHC v SetGHC_XYZ (Just tmp)
          void $ liftE $ setGHC v SetGHCOnly (Just tmp)
        Cabal -> do
          bin  <- liftE $ whereIsTool Cabal v
          cbin <- liftIO $ canonicalizePath bin
          lift $ createLink (relativeSymlink tmp cbin) (tmp </> ("cabal" <.> exeExt))
        Stack -> do
          bin  <- liftE $ whereIsTool Stack v
          cbin <- liftIO $ canonicalizePath bin
          lift $ createLink (relativeSymlink tmp cbin) (tmp </> ("stack" <.> exeExt))
        HLS -> do
          Dirs {..}  <- getDirs
          let v' = _tvVersion v
          legacy <- isLegacyHLS v'
          if legacy
          then do
            -- TODO: factor this out
            hlsWrapper <- liftE @_ @'[NotInstalled] $ hlsWrapperBinary v' !? (NotInstalled HLS (mkTVer v'))
            cw <- liftIO $ canonicalizePath (binDir </> hlsWrapper)
            lift $ createLink (relativeSymlink tmp cw) (tmp </> takeFileName cw)
            hlsBins <- hlsServerBinaries v' Nothing >>= liftIO . traverse (canonicalizePath . (binDir </>))
            forM_ hlsBins $ \bin ->
              lift $ createLink (relativeSymlink tmp bin) (tmp </> takeFileName bin)
            liftE $ setHLS (_tvVersion v) SetHLSOnly (Just tmp)
          else do
            liftE $ setHLS (_tvVersion v) SetHLS_XYZ (Just tmp)
            liftE $ setHLS (_tvVersion v) SetHLSOnly (Just tmp)
        GHCup -> pure ()
       
   addToPath path = do
    cEnv <- Map.fromList <$> getEnvironment
    let paths          = ["PATH", "Path"]
        curPaths       = (\x -> maybe [] splitSearchPath (Map.lookup x cEnv)) =<< paths
        newPath        = intercalate [searchPathSeparator] (if runAppendPATH then (curPaths ++ [path]) else (path : curPaths))
        envWithoutPath = foldr (\x y -> Map.delete x y) cEnv paths
        pathVar        = if isWindows then "Path" else "PATH"
        envWithNewPath = Map.toList $ Map.insert pathVar newPath envWithoutPath
    liftIO $ setEnv pathVar newPath
    return envWithNewPath

   predictableTmpDir (Toolchain Nothing Nothing Nothing Nothing) =
     liftIO (getTemporaryDirectory >>= \tmp -> pure (tmp </> "ghcup-none"))
   predictableTmpDir Toolchain{..} = do
      tmp <- getTemporaryDirectory
      pure $ tmp
        </> ("ghcup-" <> intercalate "_"
              (  maybe [] ( (:[]) . ("ghc-"   <>) . T.unpack . tVerToText) ghcVer
              <> maybe [] ( (:[]) . ("cabal-" <>) . T.unpack . tVerToText) cabalVer
              <> maybe [] ( (:[]) . ("hls-"   <>) . T.unpack . tVerToText) hlsVer
              <> maybe [] ( (:[]) . ("stack-" <>) . T.unpack . tVerToText) stackVer
              )
            )



    -------------------------
    --[ Other local types ]--
    -------------------------



data Toolchain = Toolchain
  { ghcVer     :: Maybe GHCTargetVersion
  , cabalVer   :: Maybe GHCTargetVersion
  , hlsVer     :: Maybe GHCTargetVersion
  , stackVer   :: Maybe GHCTargetVersion
  }
