{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module GHCup.OptParse.Run where


import GHCup.Command.Install
import GHCup.Command.Install.LowLevel
import GHCup.Command.Set
import GHCup.Errors
import GHCup.Input.Parsers
import GHCup.Input.SymlinkSpec
import GHCup.Legacy.HLS               ( setHLS )
import GHCup.Legacy.Utils
import GHCup.OptParse.Common
import GHCup.Prelude
import GHCup.Query.DB
import GHCup.Query.GHCupDirs
import GHCup.System.Directory
import GHCup.Types
import GHCup.Types.Optics
#ifdef IS_WINDOWS
import Data.Maybe                    ( fromMaybe )
import GHCup.Prelude.Process
import GHCup.Prelude.Process.Windows ( execNoMinGW, resolveExecutable )
#endif
import GHCup.Prelude.String.QQ

import Control.Exception.Safe ( displayException, handle )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad                ( forM, forM_, unless, when )
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Functor
import Data.List                    ( intercalate )
import Data.Maybe                   ( isNothing )
import Data.Variant.Excepts
import Options.Applicative          hiding ( ParseError, style )
import Prelude                      hiding ( appendFile )
import System.Environment
import System.Exit
import System.FilePath

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
#ifndef IS_WINDOWS
import qualified System.Posix.Process as SPP
#endif
import Data.Versions                  ( Version, prettyVer )
import Text.PrettyPrint.HughesPJClass ( prettyShow )





    ---------------
    --[ Options ]--
    ---------------


data RunOptions = RunOptions
  { runAppendPATH :: Bool
  , runInstTool' :: Bool
  , runMinGWPath :: Bool
  , runGHCVer :: Maybe ToolVersion
  , runCabalVer :: Maybe ToolVersion
  , runHLSVer :: Maybe ToolVersion
  , runStackVer :: Maybe ToolVersion
  , runToolVer :: [(Tool, ToolVersion)]
  , runBinDir :: Maybe FilePath
  , runQuick :: Bool
  , runCOMMAND :: [String]
  }
  deriving (Eq, Show)



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
            <> completer (tagCompleter ghc [])
            <> completer (versionCompleter [] ghc)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "CABAL_VERSION" <> long "cabal" <> help "The cabal version"
            <> completer (tagCompleter cabal [])
            <> completer (versionCompleter [] cabal)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "HLS_VERSION" <> long "hls" <> help "The HLS version"
            <> completer (tagCompleter hls [])
            <> completer (versionCompleter [] hls)
            )
          )
    <*> optional
          (option
            (eitherReader toolVersionTagEither)
            (metavar "STACK_VERSION" <> long "stack" <> help "The stack version"
            <> completer (tagCompleter stack [])
            <> completer (versionCompleter [] stack)
            )
          )
    <*> many
         ( option
            (eitherReader toolAndVersionParser)
            (metavar "TOOL,VERSION" <> long "tool" <> help "The tool and the version (separated by ',')"
            <> completer (tagCompleter stack [])
            <> completer (versionCompleter [] stack)
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
                   , URIParseError
                   , NoInstallInfo
                   , ParseError
                   , IncompatibleConfig
                   , MalformedInstallInfo
                   ]

runLeanRUN :: (MonadIOish m)
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
       ( MonadIOish m
       , Alternative m
       )
   => RunOptions
   -> Settings
   -> IO AppState
   -> LeanAppState
   -> (ReaderT LeanAppState m () -> m ())
   -> m ExitCode
run RunOptions{..} settings runAppState leanAppstate runLogger = do
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
         VRight (fromInstallDir -> tmp) -> do
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
               resolvedCmd <- fmap (fromMaybe cmd) $ liftIO $ resolveExecutable cmd runMinGWPath
               r' <- if runMinGWPath
                     then runLeanRUN leanAppstate $ liftE $ lEM @_ @'[ProcessError] $ exec resolvedCmd args Nothing (Just newEnv)
                     else runLeanRUN leanAppstate $ liftE $ lEM @_ @'[ProcessError] $ execNoMinGW resolvedCmd args Nothing (Just newEnv)
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

   guessMode = if guessVersion settings then GLaxWithInstalled else GStrict

   -- TODO: doesn't work for cross
   resolveToolchainFull :: ( MonadIOish m )
                        => Excepts
                             '[ TagNotFound
                              , DayNotFound
                              , NextVerNotFound
                              , NoToolVersionSet
                              , ParseError
                              ] (ResourceT (ReaderT AppState m)) Toolchain
   resolveToolchainFull = do
         ghcVer <- forM runGHCVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) guessMode ghc
           pure v
         cabalVer <- forM runCabalVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) guessMode cabal
           pure (_tvVersion v)
         hlsVer <- forM runHLSVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) guessMode hls
           pure (_tvVersion v)
         stackVer <- forM runStackVer $ \ver -> do
           (v, _) <- liftE $ fromVersion (Just ver) guessMode stack
           pure (_tvVersion v)
         toolVer <- forM runToolVer $ \(tool, ver) -> do
           (v, _) <- liftE $ fromVersion (Just ver) guessMode tool
           pure (tool, _tvVersion v)
         pure Toolchain{..}

   resolveToolchain = do
         ghcVer <- case runGHCVer of
            Just (GHCVersion v) -> pure $ Just v
            Just (ToolVersion v) -> pure $ Just (mkTVer v)
            Nothing -> pure Nothing
            _ -> throwE $ IncompatibleConfig "Cannot resolve tags/dates in quick mode"
         cabalVer <- case runCabalVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> throwE $ IncompatibleConfig "Cannot resolve tags/dates in quick mode"
         hlsVer <- case runHLSVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> throwE $ IncompatibleConfig "Cannot resolve tags/dates in quick mode"
         stackVer <- case runStackVer of
            Just (GHCVersion v) -> pure $ Just (_tvVersion v)
            Just (ToolVersion v) -> pure $ Just v
            Nothing -> pure Nothing
            _ -> throwE $ IncompatibleConfig "Cannot resolve tags/dates in quick mode"
         toolVer <- forM runToolVer $ \(tool, tver) -> case tver of
            (GHCVersion v) -> pure (tool, _tvVersion v)
            (ToolVersion v) -> pure (tool, v)
            _ -> throwE $ IncompatibleConfig "Cannot resolve tags/dates in quick mode"
         pure Toolchain{..}

   installToolChainFull :: ( MonadIOish m
                           , Alternative m
                           )
                        => Toolchain
                        -> InstallDirResolved
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
                              , URIParseError
                              , NoInstallInfo
                              , ParseError
                              , FileDoesNotExistError
                              , MalformedInstallInfo
                              ] (ResourceT (ReaderT AppState m)) ()
   installToolChainFull Toolchain{..} tmp = do
         case ghcVer of
           Just v -> do
             isInst <- lift $ isInstalled ghc v
             unless isInst $ when (runInstTool' && isNothing (_tvTarget v)) $ void $ liftE $ installTool
               ghc
               v
               GHCupInternal
               False
               []
               Nothing
             liftE $ setTool' ghc v tmp
           _ -> pure ()
         case cabalVer of
           Just v -> do
             isInst <- lift $ isInstalled cabal (mkTVer v)
             unless isInst $ when runInstTool' $ void $ liftE $ installTool
               cabal
               (mkTVer v)
               GHCupInternal
               False
               []
               Nothing
             liftE $ setTool' cabal (mkTVer v) tmp
           _ -> pure ()
         case stackVer of
           Just v -> do
             isInst <- lift $ isInstalled stack (mkTVer v)
             unless isInst $ when runInstTool' $ void $ liftE $ installTool
               stack
               (mkTVer v)
               GHCupInternal
               False
               []
               Nothing
             liftE $ setTool' stack (mkTVer v) tmp
           _ -> pure ()
         case hlsVer of
           Just v -> do
             isInst <- lift $ isInstalled hls (mkTVer v)
             unless isInst $ when runInstTool' $ void $ liftE $ installTool
               hls
               (mkTVer v)
               GHCupInternal
               False
               []
               Nothing
             liftE $ setTool' hls (mkTVer v) tmp
           _ -> pure ()
         forM_ toolVer $ \(t, mkTVer -> v) -> do
           isInst <- lift $ isInstalled t v
           unless isInst $ when (runInstTool' && isNothing (_tvTarget v)) $ void $ liftE $ installTool
             t
             v
             GHCupInternal
             False
             []
             Nothing
           liftE $ setTool' t v tmp

   installToolChain :: ( MonadIOish m
                       )
                    => Toolchain
                    -> InstallDirResolved
                    -> Excepts '[ParseError, NotInstalled, MalformedInstallInfo] (ReaderT LeanAppState m) ()
   installToolChain Toolchain{..} tmp = do
         case ghcVer of
           Just v -> setTool' ghc v tmp
           _      -> pure ()
         case cabalVer of
           Just v -> setTool' cabal (mkTVer v) tmp
           _      -> pure ()
         case stackVer of
           Just v -> setTool' stack (mkTVer v) tmp
           _      -> pure ()
         case hlsVer of
           Just v -> setTool' hls (mkTVer v) tmp
           _      -> pure ()
         forM_ toolVer $ \(t, mkTVer -> v) -> setTool' t v tmp

   setTool' ::
     forall m1 env .
     ( MonadReader env m1
     , HasDirs env
     , HasPlatformReq env
     , HasLog env
     , MonadIOish m1
     )
     => Tool
     -> TargetVersion
     -> InstallDirResolved
     -> Excepts '[ParseError, NotInstalled, MalformedInstallInfo] m1 ()
   setTool' tool v tmp = do
     lift (runE @'[FileDoesNotExistError, ParseError, NoInstallInfo] (getSymlinkSpec' tool v)) >>= \case
       VRight symSpec -> do
         let tmp' = fromInstallDir tmp
         dest <- lift $ toolInstallDestination tool v
         liftE $ void $ symlinkBinaries (GHCupDir dest) symSpec tmp tool v
         liftE $ void $ setToolVersion' tool v (Just tmp')
       VLeft (V pe@(ParseError _)) -> fail $ prettyHFError pe
       VLeft _ -- legacy
         | tool == ghc -> do
             let tmp' = fromInstallDir tmp
             pfreq <- lift getPlatformReq
             symSpec <- forM (defaultGHCExeSymLinked pfreq v (ghcBinaries pfreq v)) (liftE . parseSymlinkSpec (_tvVersion v))
             dest <- lift $ toolInstallDestination tool v
             liftE $ void $ symlinkBinaries (GHCupDir dest) symSpec tmp tool v
             liftE $ void $ setToolVersion' tool v (Just tmp')
         -- we can't use 'symlinkBinaries' here for most tools, because
         -- it relies on everything residing within @~/.ghcup/<tool>@,
         -- which is not the case for early cabal/stack/hls
         | tool == cabal -> legacySet'
         | tool == stack -> legacySet'
         | tool == hls -> do
             let tmp' = fromInstallDir tmp
             setHLS' (_tvVersion v) tmp'
         | tool == ghcup -> do
             pure ()
         | otherwise ->
             throwE $ NotInstalled tool v
     pure ()
    where
     legacySet' = do
       let tmp' = fromInstallDir tmp
       Dirs {..}  <- getDirs
       let tool' = prettyShow tool
           pvpExe = tool' <> "-" <> prettyShow v <.> exeExt

       -- create <tool>-X.Y.Z
       target <- binarySymLinkDestination tmp' (binDir </> pvpExe)
       lift $ createLink target (tmp' </> pvpExe)

       -- create <tool>-X.Y
       lift $ handle
                (\(e :: ParseError) -> logWarn (T.pack $ displayException e))
             $ do
                (mj, mi) <- getMajorMinorV (_tvVersion v)
                let exeMajorMinor = tool' <> "-" <> T.unpack (intToText mj) <> "." <> T.unpack (intToText mi) <.> exeExt
                createLink pvpExe (tmp' </> exeMajorMinor)

       -- create <tool>
       liftE $ void $ setToolVersion' tool v (Just tmp')


   -- TODO: legacy
   setHLS' v tmp = do
          Dirs {..}  <- getDirs
          legacy <- isLegacyHLS v
          if legacy
          then do
            -- TODO: factor this out
            hlsWrapper <- liftE @_ @'[NotInstalled] $ hlsWrapperBinary v !? NotInstalled hls (mkTVer v)
            cw <- liftIO $ canonicalizePath (binDir </> hlsWrapper)
            lift $ createLink (relativeSymlink tmp cw) (tmp </> takeFileName cw)
            hlsBins <- hlsServerBinaries v Nothing >>= liftIO . traverse (canonicalizePath . (binDir </>))
            forM_ hlsBins $ \bin ->
              lift $ createLink (relativeSymlink tmp bin) (tmp </> takeFileName bin)
            liftE $ setHLS v SetHLSOnly (Just tmp)
          else do
            liftE $ void $ setToolVersion' hls (mkTVer v) (Just tmp)

   createTmpDir :: ( MonadIOish m )
                => Toolchain
                -> ReaderT LeanAppState m InstallDirResolved
   createTmpDir toolchain =
     case runBinDir of
           Just bindir -> do
             liftIO $ createDirRecursive' bindir
             fmap IsolateDirResolved $ liftIO $ canonicalizePath bindir
           Nothing -> do
             d <- predictableTmpDir toolchain
             liftIO $ createDirRecursive' (fromGHCupPath d)
             pure $ GHCupDir d

   predictableTmpDir :: Monad m
                     => Toolchain
                     -> ReaderT LeanAppState m GHCupPath
   predictableTmpDir (Toolchain Nothing Nothing Nothing Nothing []) = do
     Dirs { tmpDir } <- getDirs
     pure (tmpDir `appendGHCupPath` "ghcup-none")
   predictableTmpDir Toolchain{..} = do
      Dirs { tmpDir } <- getDirs
      pure $ tmpDir
        `appendGHCupPath` ("ghcup-" <> intercalate "_"
              (  maybe [] ( (:[]) . ("ghc-"   <>) . T.unpack . tVerToText) ghcVer
              <> maybe [] ( (:[]) . ("cabal-" <>) . T.unpack . prettyVer) cabalVer
              <> maybe [] ( (:[]) . ("hls-"   <>) . T.unpack . prettyVer) hlsVer
              <> maybe [] ( (:[]) . ("stack-" <>) . T.unpack . prettyVer) stackVer
              <> fmap (\(t, v) -> prettyShow t <> "-" <> T.unpack (prettyVer v)) toolVer
              )
            )



    -------------------------
    --[ Other local types ]--
    -------------------------



data Toolchain = Toolchain
  { ghcVer :: Maybe TargetVersion
  , cabalVer :: Maybe Version
  , hlsVer :: Maybe Version
  , stackVer :: Maybe Version
  , toolVer :: [(Tool, Version)]
  }
  deriving (Show)
