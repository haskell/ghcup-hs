{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : GHCup.Utils.File.Posix
Description : File and unix APIs
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX

This module handles file and executable handling.
Some of these functions use sophisticated logging.
-}
module GHCup.Utils.File.Posix where

import           GHCup.Utils.File.Common
import           GHCup.Utils.Prelude
import           GHCup.Types

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception              ( evaluate )
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.IORef
import           Data.Sequence                  ( Seq, (|>) )
import           Data.String.Interpolate
import           Data.List
import           Data.Word8
import           GHC.IO.Exception
import           System.Console.Pretty   hiding ( Pretty )
import           System.Console.Regions
import           System.IO.Error
import           System.FilePath
import           System.Posix.Directory
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Process           ( ProcessStatus(..) )
import           System.Posix.Types


import qualified Control.Exception             as EX
import qualified Data.Sequence                 as Sq
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified System.Posix.Process          as SPP
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified "unix-bytestring" System.Posix.IO.ByteString
                                               as SPIB



-- | Execute the given command and collect the stdout, stderr and the exit code.
-- The command is run in a subprocess.
executeOut :: FilePath          -- ^ command as filename, e.g. 'ls'
           -> [String]          -- ^ arguments to the command
           -> Maybe FilePath    -- ^ chdir to this path
           -> IO CapturedProcess
executeOut path args chdir = captureOutStreams $ do
  maybe (pure ()) changeWorkingDirectory chdir
  SPP.executeFile path True args Nothing


execLogged :: (MonadReader AppState m, MonadIO m, MonadThrow m)
           => FilePath         -- ^ thing to execute
           -> [String]         -- ^ args for the thing
           -> Maybe FilePath   -- ^ optionally chdir into this
           -> FilePath         -- ^ log filename (opened in append mode)
           -> Maybe [(String, String)] -- ^ optional environment
           -> m (Either ProcessError ())
execLogged exe args chdir lfile env = do
  AppState { settings = Settings {..}, dirs = Dirs {..} } <- ask
  let logfile = logsDir </> lfile <> ".log"
  liftIO $ bracket (openFd logfile WriteOnly (Just newFilePerms) defaultFileFlags{ append = True })
                   closeFd
                   (action verbose)
 where
  action verbose fd = do
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
              else printToRegion fd stdoutRead 6 pState
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
  tee fileFd fdIn = readTilEOF lineAction fdIn

   where
    lineAction :: ByteString -> IO ()
    lineAction bs' = do
      void $ SPIB.fdWrite fileFd (bs' <> "\n")
      void $ SPIB.fdWrite stdOutput (bs' <> "\n")

  -- Reads fdIn and logs the output in a continous scrolling area
  -- of 'size' terminal lines. Also writes to a log file.
  printToRegion :: Fd -> Fd -> Int -> MVar Bool -> IO ()
  printToRegion fileFd fdIn size pState = do
    void $ displayConsoleRegions $ do
      rs <-
        liftIO
        . fmap Sq.fromList
        . sequence
        . replicate size
        . openConsoleRegion
        $ Linear
      flip runStateT mempty
        $ handle
            (\(ex :: SomeException) -> do
              ps <- liftIO $ takeMVar pState
              when ps (forM_ rs (liftIO . closeConsoleRegion))
              throw ex
            )
        $ readTilEOF (lineAction rs) fdIn

   where
    -- action to perform line by line
    -- TODO: do this with vty for efficiency
    lineAction :: (MonadMask m, MonadIO m)
               => Seq ConsoleRegion
               -> ByteString
               -> StateT (Seq ByteString) m ()
    lineAction rs = \bs' -> do
      void $ liftIO $ SPIB.fdWrite fileFd (bs' <> "\n")
      modify (swapRegs bs')
      regs <- get
      liftIO $ forM_ (Sq.zip regs rs) $ \(bs, r) -> setConsoleRegion r $ do
        w <- consoleWidth
        return
          . T.pack
          . color Blue
          . T.unpack
          . decUTF8Safe
          . trim w
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
        void $ evaluate a

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
  openFd dest WriteOnly (Just fm) defaultFileFlags{ exclusive = True }


-- | Thin wrapper around `executeFile`.
exec :: String           -- ^ thing to execute
     -> [String]         -- ^ args for the thing
     -> Maybe FilePath   -- ^ optionally chdir into this
     -> Maybe [(String, String)] -- ^ optional environment
     -> IO (Either ProcessError ())
exec exe args chdir env = do
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



chmod_755 :: (MonadLogger m, MonadIO m) => FilePath -> m ()
chmod_755 fp = do
  let exe_mode =
          nullFileMode
            `unionFileModes` ownerExecuteMode
            `unionFileModes` ownerReadMode
            `unionFileModes` ownerWriteMode
            `unionFileModes` groupExecuteMode
            `unionFileModes` groupReadMode
            `unionFileModes` otherExecuteMode
            `unionFileModes` otherReadMode
  $(logDebug) [i|chmod 755 #{fp}|]
  liftIO $ setFileMode fp exe_mode


-- |Default permissions for a new file.
newFilePerms :: FileMode
newFilePerms =
  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode
