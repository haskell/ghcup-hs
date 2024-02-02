{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE MultiWayIf  #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : GHCup.Utils.File.Posix
Description : Process handling for unix
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Prelude.Process.Posix where

import           GHCup.Utils.Dirs
import           GHCup.Prelude.File
import           GHCup.Prelude.File.Posix
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Types
import           GHCup.Types.Optics

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Control.Exception              as E
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.IORef
import           Data.Sequence                  ( Seq, (|>) )
import           Data.List
import           Data.Word8
import           GHC.IO.Exception
import           System.IO                      ( stderr )
import           System.IO.Error      hiding    ( catchIOError )
import           System.FilePath
import           System.Posix.Directory
import           System.Posix.IO
import           System.Posix.Process           ( ProcessStatus(..) )
import           System.Posix.Types


import qualified Control.Exception             as EX
import qualified Data.Sequence                 as Sq
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified System.Posix.Process          as SPP
import qualified System.Console.Terminal.Size  as TP
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified System.Posix.IO.ByteString.Ext
                                               as SPIB



-- | Execute the given command and collect the stdout, stderr and the exit code.
-- The command is run in a subprocess.
executeOut :: MonadIO m
           => FilePath          -- ^ command as filename, e.g. 'ls'
           -> [String]          -- ^ arguments to the command
           -> Maybe FilePath    -- ^ chdir to this path
           -> m CapturedProcess
executeOut path args chdir = liftIO $ captureOutStreams $ do
  maybe (pure ()) changeWorkingDirectory chdir
  SPP.executeFile path True args Nothing

executeOut' :: MonadIO m
            => FilePath          -- ^ command as filename, e.g. 'ls'
            -> [String]          -- ^ arguments to the command
            -> Maybe FilePath    -- ^ chdir to this path
            -> Maybe [(String, String)]
            -> m CapturedProcess
executeOut' path args chdir env = liftIO $ captureOutStreams $ do
  maybe (pure ()) changeWorkingDirectory chdir
  SPP.executeFile path True args env


execLogged :: ( MonadReader env m
              , HasSettings env
              , HasLog env
              , HasDirs env
              , MonadIO m
              , MonadThrow m)
           => FilePath         -- ^ thing to execute
           -> [String]         -- ^ args for the thing
           -> Maybe FilePath   -- ^ optionally chdir into this
           -> FilePath         -- ^ log filename (opened in append mode)
           -> Maybe [(String, String)] -- ^ optional environment
           -> m (Either ProcessError ())
execLogged exe args chdir lfile env = do
  Settings {..} <- getSettings
  Dirs {..} <- getDirs
  logDebug $ T.pack $ "Running " <> exe <> " with arguments " <> show args
  let logfile = fromGHCupPath logsDir </> lfile <> ".log"
  liftIO $ bracket
#if MIN_VERSION_unix(2,8,0)
                   (openFd logfile WriteOnly defaultFileFlags{ append = True, creat = Just newFilePerms })
#else
                   (openFd logfile WriteOnly (Just newFilePerms) defaultFileFlags{ append = True })
#endif
                   closeFd
                   (action verbose noColor)
 where
  action verbose no_color fd = do
    actionWithPipes $ \(stdoutRead, stdoutWrite) -> do
      -- start the thread that logs to stdout
      pState <- newEmptyMVar
      done   <- newEmptyMVar
      void
        $ forkIO
        $ EX.handle (\(_ :: IOException) -> pure ())
        $ EX.finally
            (if verbose
              then tee fd stdoutRead
              else printToRegion fd stdoutRead 6 pState no_color
            )
            (putMVar done ())

      -- fork the subprocess
      pid <- SPP.forkProcess $ do
        void $ dupTo stdoutWrite stdOutput
        void $ dupTo stdoutWrite stdError
        closeFd stdoutRead
        closeFd stdoutWrite

        -- execute the action
        maybe (pure ()) changeWorkingDirectory chdir
        void $ SPP.executeFile exe (not ("./" `isPrefixOf` exe)) args env

      closeFd stdoutWrite

      -- wait for the subprocess to finish
      e <- toProcessError exe args <$!> SPP.getProcessStatus True True pid
      putMVar pState (either (const False) (const True) e)

      void $ race (takeMVar done) (threadDelay (1000000 * 3))
      closeFd stdoutRead

      pure e

  tee :: Fd -> Fd -> IO ()
  tee fileFd = readTilEOF lineAction

   where
    lineAction :: ByteString -> IO ()
    lineAction bs' = do
      void $ SPIB.fdWrite fileFd (bs' <> "\n")
      void $ SPIB.fdWrite stdOutput (bs' <> "\n")

  -- Reads fdIn and logs the output in a continuous scrolling area
  -- of 'size' terminal lines. Also writes to a log file.
  printToRegion :: Fd -> Fd -> Int -> MVar Bool -> Bool -> IO ()
  printToRegion fileFd fdIn size pState no_color = do
    -- init region
    forM_ [1..size] $ \_ -> BS.hPut stderr "\n"

    void $ flip runStateT mempty
      $ do
        handle
          (\(ex :: SomeException) -> do
            ps <- liftIO $ takeMVar pState
            when ps (liftIO $ BS.hPut stderr (pos1 <> moveLineUp size <> clearScreen))
            throw ex
          ) $ readTilEOF lineAction fdIn

   where
    clearScreen :: ByteString
    clearScreen = "\x1b[0J"
    clearLine :: ByteString
    clearLine = "\x1b[2K"
    moveLineUp :: Int -> ByteString
    moveLineUp n = "\x1b[" <> E.encodeUtf8 (T.pack (show n)) <> "A"
    moveLineDown :: Int -> ByteString
    moveLineDown n = "\x1b[" <> E.encodeUtf8 (T.pack (show n)) <> "B"
    pos1 :: ByteString
    pos1 = "\r"
    overwriteNthLine :: Int -> ByteString -> ByteString
    overwriteNthLine n str = pos1 <> moveLineUp n <> clearLine <> str <> moveLineDown n <> pos1

    blue :: ByteString -> ByteString
    blue bs
      | no_color = bs
      | otherwise = "\x1b[0;34m" <> bs <> "\x1b[0m"

    -- action to perform line by line
    lineAction :: (MonadMask m, MonadIO m)
               => ByteString
               -> StateT (Seq ByteString) m ()
    lineAction = \bs' -> do
      void $ liftIO $ SPIB.fdWrite fileFd (bs' <> "\n")
      modify (swapRegs bs')
      liftIO TP.size >>= \case
        Nothing -> pure ()
        Just (TP.Window _ w) -> do
          regs <- get
          liftIO $ forM_ (Sq.zip regs (Sq.fromList [0..(Sq.length regs - 1)])) $ \(bs, i) -> do
              BS.hPut stderr
              . overwriteNthLine (size - i)
              . trim w
              . blue
              . (\b -> "[ " <> E.encodeUtf8 (T.pack lfile) <> " ] " <> b)
              $ bs

    swapRegs :: a -> Seq a -> Seq a
    swapRegs bs = \regs -> if
      | Sq.length regs < size -> regs |> bs
      | otherwise             -> Sq.drop 1 regs |> bs

    -- trim output line to terminal width
    trim :: Int -> ByteString -> ByteString
    trim w = \bs -> if
      | BS.length bs > w && w > 5 -> BS.take (w - 4) bs <> "..."
      | otherwise                 -> bs

  -- Consecutively read from Fd in 512 chunks until we hit
  -- newline or EOF.
  readLine :: MonadIO m
           => Fd          -- ^ input file descriptor
           -> ByteString  -- ^ rest buffer (read across newline)
           -> m (ByteString, ByteString, Bool) -- ^ (full line, rest, eof)
  readLine fd = go
   where
    go inBs = do
      -- if buffer is not empty, process it first
      mbs <- if BS.length inBs == 0
               -- otherwise attempt read
               then liftIO
                    $ handleIO (\e -> if isEOFError e then pure Nothing else ioError e)
                    $ fmap Just
                    $ SPIB.fdRead fd 512
               else pure $ Just inBs
      case mbs of
        Nothing -> pure ("", "", True)
        Just bs -> do
          -- split on newline
          let (line, rest) = BS.span (/= _lf) bs
          if
            | BS.length rest /= 0 -> pure (line, BS.tail rest, False)
            -- if rest is empty, then there was no newline, process further
            | otherwise           -> (\(l, r, b) -> (line <> l, r, b)) <$!> go mempty

  readTilEOF :: MonadIO m => (ByteString -> m a) -> Fd -> m ()
  readTilEOF ~action' fd' = go mempty
   where
    go bs' = do
      (bs, rest, eof) <- readLine fd' bs'
      if eof
         then liftIO $ ioError (mkIOError eofErrorType "" Nothing Nothing)
         else void (action' bs) >> go rest


-- | Capture the stdout and stderr of the given action, which
-- is run in a subprocess. Stdin is closed. You might want to
-- 'race' this to make sure it terminates.
captureOutStreams :: IO a
                     -- ^ the action to execute in a subprocess
                  -> IO CapturedProcess
captureOutStreams action = do
  actionWithPipes $ \(parentStdoutRead, childStdoutWrite) ->
    actionWithPipes $ \(parentStderrRead, childStderrWrite) -> do
      pid <- SPP.forkProcess $ do
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
        void $ E.evaluate a

      -- close everything we don't need
      closeFd childStdoutWrite
      closeFd childStderrWrite

      -- start thread that writes the output
      refOut <- newIORef BL.empty
      refErr <- newIORef BL.empty
      done   <- newEmptyMVar
      _      <-
        forkIO
        $ EX.handle (\(_ :: IOException) -> pure ())
        $ flip EX.finally (putMVar done ())
        $ writeStds parentStdoutRead parentStderrRead refOut refErr

      status <- SPP.getProcessStatus True True pid
      void $ race (takeMVar done) (threadDelay (1000000 * 3))

      case status of
        -- readFd will take care of closing the fd
        Just (SPP.Exited es) -> do
          stdout' <- readIORef refOut
          stderr' <- readIORef refErr
          pure $ CapturedProcess { _exitCode = es
                                 , _stdOut   = stdout'
                                 , _stdErr   = stderr'
                                 }

        _ -> throwIO $ userError ("No such PID " ++ show pid)

 where
  writeStds :: Fd -> Fd -> IORef BL.ByteString -> IORef BL.ByteString -> IO ()
  writeStds pout perr rout rerr = do
    doneOut <- newEmptyMVar
    void
      $ forkIO
      $ hideError eofErrorType
      $ flip EX.finally (putMVar doneOut ())
      $ readTilEOF (\x -> modifyIORef' rout (<> BL.fromStrict x)) pout
    doneErr <- newEmptyMVar
    void
      $ forkIO
      $ hideError eofErrorType
      $ flip EX.finally (putMVar doneErr ())
      $ readTilEOF (\x -> modifyIORef' rerr (<> BL.fromStrict x)) perr
    takeMVar doneOut
    takeMVar doneErr

  readTilEOF ~action' fd' = do
    bs <- SPIB.fdRead fd' 512
    void $ action' bs
    readTilEOF action' fd'


actionWithPipes :: ((Fd, Fd) -> IO b) -> IO b
actionWithPipes a =
  createPipe >>= \(p1, p2) -> flip finally (cleanup [p1, p2]) $ a (p1, p2)

cleanup :: [Fd] -> IO ()
cleanup fds = for_ fds $ \fd -> handleIO (\_ -> pure ()) $ closeFd fd



-- | Create a new regular file in write-only mode. The file must not exist.
createRegularFileFd :: FileMode -> FilePath -> IO Fd
createRegularFileFd fm dest =
#if MIN_VERSION_unix(2,8,0)
  openFd dest WriteOnly defaultFileFlags{ exclusive = True, creat = Just fm }
#else
  openFd dest WriteOnly (Just fm) defaultFileFlags{ exclusive = True }
#endif


-- | Thin wrapper around `executeFile`.
exec :: MonadIO m
     => String           -- ^ thing to execute
     -> [String]         -- ^ args for the thing
     -> Maybe FilePath   -- ^ optionally chdir into this
     -> Maybe [(String, String)] -- ^ optional environment
     -> m (Either ProcessError ())
exec exe args chdir env = liftIO $ do
  pid <- SPP.forkProcess $ do
    maybe (pure ()) changeWorkingDirectory chdir
    SPP.executeFile exe (not ("./" `isPrefixOf` exe)) args env

  fmap (toProcessError exe args) $ SPP.getProcessStatus True True pid


toProcessError :: FilePath
               -> [String]
               -> Maybe ProcessStatus
               -> Either ProcessError ()
toProcessError exe args mps = case mps of
  Just (SPP.Exited (ExitFailure xi)) -> Left $ NonZeroExit xi exe args
  Just (SPP.Exited ExitSuccess    ) -> Right ()
  Just (Terminated _ _             ) -> Left $ PTerminated exe args
  Just (Stopped _                  ) -> Left $ PStopped exe args
  Nothing                            -> Left $ NoSuchPid exe args



