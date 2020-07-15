{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}

module GHCup.Utils.File where

import           GHCup.Utils.Dirs
import           GHCup.Utils.Prelude
import           GHCup.Types

import           Control.Concurrent
import           Control.Exception              ( evaluate )
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.IO.Exception
import           HPath
import           HPath.IO                hiding ( hideError )
import           Optics
import           System.Console.Pretty
import           System.Console.Regions
import           System.IO.Error
import           System.Posix.Directory.ByteString
import           System.Posix.FD               as FD
import           System.Posix.FilePath   hiding ( (</>) )
import           System.Posix.Foreign           ( oExcl )
import "unix"    System.Posix.IO.ByteString
                                         hiding ( openFd )
import           System.Posix.Process           ( ProcessStatus(..) )
import           System.Posix.Types
import           Text.Regex.Posix


import qualified Control.Exception             as EX
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified System.Posix.Process.ByteString
                                               as SPPB
import           Streamly.External.Posix.DirStream
import qualified Streamly.Prelude              as S
import qualified Text.Megaparsec               as MP
import qualified Data.ByteString               as BS
import qualified "unix-bytestring" System.Posix.IO.ByteString
                                               as SPIB




-- | Bool signals whether the regions should be cleaned.
data StopThread = StopThread Bool
  deriving Show

instance Exception StopThread


data ProcessError = NonZeroExit Int ByteString [ByteString]
                  | PTerminated ByteString [ByteString]
                  | PStopped ByteString [ByteString]
                  | NoSuchPid ByteString [ByteString]
                  deriving Show


data CapturedProcess = CapturedProcess
  { _exitCode :: ExitCode
  , _stdOut   :: ByteString
  , _stdErr   :: ByteString
  }
  deriving (Eq, Show)

makeLenses ''CapturedProcess


-- | Find the given executable by searching all *absolute* PATH components.
-- Relative paths in PATH are ignored.
--
-- This shouldn't throw IO exceptions, unless getting the environment variable
-- PATH does.
findExecutable :: Path Rel -> IO (Maybe (Path Abs))
findExecutable ex = do
  sPaths <- fmap catMaybes . (fmap . fmap) parseAbs $ getSearchPath
  -- We don't want exceptions to mess up our result. If we can't
  -- figure out if a file exists, then treat it as a negative result.
  asum $ fmap (handleIO (\_ -> pure Nothing)) $ fmap
    -- asum for short-circuiting behavior
    (\s' -> (isExecutable (s' </> ex) >>= guard) $> (Just (s' </> ex)))
    sPaths


-- | Execute the given command and collect the stdout, stderr and the exit code.
-- The command is run in a subprocess.
executeOut :: Path b            -- ^ command as filename, e.g. 'ls'
           -> [ByteString]      -- ^ arguments to the command
           -> Maybe (Path Abs)  -- ^ chdir to this path
           -> IO CapturedProcess
executeOut path args chdir = captureOutStreams $ do
  maybe (pure ()) (changeWorkingDirectory . toFilePath) chdir
  SPPB.executeFile (toFilePath path) True args Nothing


execLogged :: (MonadReader Settings m, MonadIO m, MonadThrow m)
           => ByteString       -- ^ thing to execute
           -> Bool             -- ^ whether to search PATH for the thing
           -> [ByteString]     -- ^ args for the thing
           -> Path Rel         -- ^ log filename
           -> Maybe (Path Abs) -- ^ optionally chdir into this
           -> Maybe [(ByteString, ByteString)] -- ^ optional environment
           -> m (Either ProcessError ())
execLogged exe spath args lfile chdir env = do
  Settings{..} <- ask
  ldir    <- liftIO ghcupLogsDir
  logfile <- (ldir </>) <$> parseRel (toFilePath lfile <> ".log")
  liftIO $ bracket (createFile (toFilePath logfile) newFilePerms) closeFd (action verbose)
 where
  action verbose fd = do
    actionWithPipes $ \(stdoutRead, stdoutWrite) -> do
      -- start the thread that logs to stdout
      done <- newEmptyMVar
      tid  <-
        forkIO
        $ EX.handle (\(_ :: StopThread) -> pure ())
        $ EX.handle (\(_ :: IOException) -> pure ())
        $ flip finally (putMVar done ())
        $ (if verbose then tee fd stdoutRead else printToRegion fd stdoutRead 6)

      -- fork our subprocess
      pid <- SPPB.forkProcess $ do
        void $ dupTo stdoutWrite stdOutput
        void $ dupTo stdoutWrite stdError
        closeFd stdoutWrite
        closeFd stdoutRead

        -- execute the action
        maybe (pure ()) (changeWorkingDirectory . toFilePath) chdir
        SPPB.executeFile exe spath args env

      closeFd stdoutWrite

      -- wait for the subprocess to finish
      e <- toProcessError exe args <$!> SPPB.getProcessStatus True True pid

      -- make sure the logging thread stops
      case e of
        Left  _ -> EX.throwTo tid (StopThread False)
        Right _ -> EX.throwTo tid (StopThread True)
      takeMVar done

      closeFd stdoutRead
      pure e

  tee :: Fd -> Fd -> IO a
  tee fileFd fdIn = do
    hideError eofErrorType $ readTilEOF lineAction fdIn
    forever (threadDelay 5000)

   where
    lineAction :: ByteString -> IO ()
    lineAction bs' = do
      void $ SPIB.fdWrite fileFd (bs' <> "\n")
      void $ SPIB.fdWrite stdOutput (bs' <> "\n")

  -- Reads fdIn and logs the output in a continous scrolling area
  -- of 'size' terminal lines. Also writes to a log file.
  printToRegion :: Fd -> Fd -> Int -> IO ()
  printToRegion fileFd fdIn size = do
    void $ displayConsoleRegions $ do
      rs <- liftIO . sequence . replicate size . openConsoleRegion $ Linear
      flip runStateT []
        $ handle
            (\ex@(StopThread b) -> do
              hideError eofErrorType $ readTilEOF (lineAction rs) fdIn
              when b (forM_ rs (liftIO . closeConsoleRegion))
              EX.throw ex
            )
        $ do
            hideError eofErrorType $ readTilEOF (lineAction rs) fdIn
            -- wait for explicit stop from the parent to signal what cleanup to run
            forever (liftIO $ threadDelay 5000)

   where
    -- action to perform line by line
    -- TODO: do this with vty for efficiency
    lineAction :: (MonadMask m, MonadIO m)
               => [ConsoleRegion]
               -> ByteString
               -> StateT [ByteString] m ()
    lineAction rs = \bs' -> do
      void $ liftIO $ SPIB.fdWrite fileFd (bs' <> "\n")
      modify (swapRegs bs')
      regs <- get
      liftIO $ forM_ (zip regs rs) $ \(bs, r) ->
        setConsoleRegion r $ do
          w <- consoleWidth
          return
            . T.pack
            . color Blue
            . T.unpack
            . decUTF8Safe
            . trim w
            . (\b -> "[ " <> toFilePath lfile <> " ] " <> b)
            $ bs

    swapRegs :: a -> [a] -> [a]
    swapRegs bs = \regs -> if | length regs < size -> regs ++ [bs]
                              | otherwise          -> tail regs ++ [bs]

    -- trim output line to terminal width
    trim :: Int -> ByteString -> ByteString
    trim w = \bs -> if | BS.length bs > w && w > 5 -> BS.take (w - 4) bs <> "..."
                       | otherwise                 -> bs

  -- read an entire line from the file descriptor (removes the newline char)
  readLine :: MonadIO m => Fd -> m ByteString
  readLine fd' = do
    bs <- liftIO $ SPIB.fdRead fd' 1
    if
      | bs == "\n" -> pure ""
      | bs == ""   -> pure ""
      | otherwise  -> (bs <>) <$!> readLine fd'

  readTilEOF :: MonadIO m => (ByteString -> m a) -> Fd -> m b
  readTilEOF action' fd' = do
    bs <- readLine fd'
    void $ action' bs
    readTilEOF action' fd'


-- | Capture the stdout and stderr of the given action, which
-- is run in a subprocess. Stdin is closed. You might want to
-- 'race' this to make sure it terminates.
captureOutStreams :: IO a
                     -- ^ the action to execute in a subprocess
                  -> IO CapturedProcess
captureOutStreams action = do
  actionWithPipes $ \(parentStdoutRead, childStdoutWrite) ->
    actionWithPipes $ \(parentStderrRead, childStderrWrite) -> do
      pid <- SPPB.forkProcess $ do
        -- dup stdout
        void $ dupTo childStdoutWrite stdOutput
        closeFd childStdoutWrite
        closeFd parentStdoutRead

        -- dup stderr
        void $ dupTo childStderrWrite stdError
        closeFd childStderrWrite
        closeFd parentStderrRead

        -- execute the action
        a <- action
        void $ evaluate a

      -- close everything we don't need
      closeFd childStdoutWrite
      closeFd childStderrWrite

      -- start thread that writes the output
      refOut <- newIORef BS.empty
      refErr <- newIORef BS.empty
      done   <- newEmptyMVar
      _      <-
        forkIO
        $ EX.handle (\(_ :: StopThread) -> pure ())
        $ EX.handle (\(_ :: IOException) -> pure ())
        $ flip finally (putMVar done ())
        $ writeStds parentStdoutRead parentStderrRead refOut refErr

      status <- SPPB.getProcessStatus True True pid
      takeMVar done

      case status of
        -- readFd will take care of closing the fd
        Just (SPPB.Exited es) -> do
          stdout' <- readIORef refOut
          stderr' <- readIORef refErr
          pure $ CapturedProcess { _exitCode = es
                                 , _stdOut   = stdout'
                                 , _stdErr   = stderr'
                                 }

        _ -> throwIO $ userError $ ("No such PID " ++ show pid)

 where
  writeStds pout perr rout rerr = do
    doneOut <- newEmptyMVar
    void
      $ forkIO
      $ hideError eofErrorType
      $ flip finally (putMVar doneOut ())
      $ readTilEOF (\x -> modifyIORef' rout (<> x)) pout
    doneErr <- newEmptyMVar
    void
      $ forkIO
      $ hideError eofErrorType
      $ flip finally (putMVar doneErr ())
      $ readTilEOF (\x -> modifyIORef' rerr (<> x)) perr
    takeMVar doneOut
    takeMVar doneErr

  readTilEOF ~action' fd' = do
    bs <- SPIB.fdRead fd' 512
    void $ action' bs
    readTilEOF action' fd'


actionWithPipes :: ((Fd, Fd) -> IO b) -> IO b
actionWithPipes a =
  createPipe >>= \(p1, p2) -> (flip finally) (cleanup [p1, p2]) $ a (p1, p2)

cleanup :: [Fd] -> IO ()
cleanup fds = for_ fds $ \fd -> handleIO (\_ -> pure ()) $ closeFd fd



-- | Create a new regular file in write-only mode. The file must not exist.
createRegularFileFd :: FileMode -> Path b -> IO Fd
createRegularFileFd fm dest =
  FD.openFd (toFilePath dest) WriteOnly [oExcl] (Just fm)


-- | Thin wrapper around `executeFile`.
exec :: ByteString       -- ^ thing to execute
     -> Bool             -- ^ whether to search PATH for the thing
     -> [ByteString]     -- ^ args for the thing
     -> Maybe (Path Abs) -- ^ optionally chdir into this
     -> Maybe [(ByteString, ByteString)] -- ^ optional environment
     -> IO (Either ProcessError ())
exec exe spath args chdir env = do
  pid <- SPPB.forkProcess $ do
    maybe (pure ()) (changeWorkingDirectory . toFilePath) chdir
    SPPB.executeFile exe spath args env

  fmap (toProcessError exe args) $ SPPB.getProcessStatus True True pid


toProcessError :: ByteString
               -> [ByteString]
               -> Maybe ProcessStatus
               -> Either ProcessError ()
toProcessError exe args mps = case mps of
  Just (SPPB.Exited (ExitFailure i)) -> Left $ NonZeroExit i exe args
  Just (SPPB.Exited ExitSuccess    ) -> Right ()
  Just (Terminated _ _             ) -> Left $ PTerminated exe args
  Just (Stopped _                  ) -> Left $ PStopped exe args
  Nothing                            -> Left $ NoSuchPid exe args


-- | Search for a file in the search paths.
--
-- Catches `PermissionDenied` and `NoSuchThing` and returns `Nothing`.
searchPath :: [Path Abs] -> Path Rel -> IO (Maybe (Path Abs))
searchPath paths needle = go paths
 where
  go [] = pure Nothing
  go (x : xs) =
    hideErrorDefM [InappropriateType, PermissionDenied, NoSuchThing] (go xs)
      $ do
          dirStream <- openDirStream (toFilePath x)
          S.findM (\(_, p) -> isMatch x p) (dirContentsStream dirStream)
            >>= \case
                  Just _  -> pure $ Just (x </> needle)
                  Nothing -> go xs
  isMatch basedir p = do
    if p == toFilePath needle
      then isExecutable (basedir </> needle)
      else pure False


findFiles :: Path Abs -> Regex -> IO [Path Rel]
findFiles path regex = do
  dirStream <- openDirStream (toFilePath path)
  f         <-
    (fmap . fmap) snd
    . S.toList
    . S.filter (\(_, p) -> match regex p)
    $ dirContentsStream dirStream
  pure $ join $ fmap parseRel f


findFiles' :: Path Abs -> MP.Parsec Void Text () -> IO [Path Rel]
findFiles' path parser = do
  dirStream <- openDirStream (toFilePath path)
  f         <-
    (fmap . fmap) snd
    . S.toList
    . S.filter (\(_, p) -> case E.decodeUtf8' p of
                             Left _ -> False
                             Right p' -> isJust $ MP.parseMaybe parser p')
    $ dirContentsStream dirStream
  pure $ join $ fmap parseRel f
