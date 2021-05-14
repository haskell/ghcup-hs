{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

{-|
Module      : GHCup.Utils.File.Windows
Description : File and windows APIs
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : Windows

This module handles file and executable handling.
Some of these functions use sophisticated logging.
-}
module GHCup.Utils.File.Windows where

import           GHCup.Utils.File.Common
import           GHCup.Types

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Foreign.C.Error
import           GHC.IO.Exception
import           GHC.IO.Handle
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
 
import qualified Control.Exception             as EX
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL



toProcessError :: FilePath
               -> [FilePath]
               -> ExitCode
               -> Either ProcessError ()
toProcessError exe args exitcode = case exitcode of
  (ExitFailure xi) -> Left $ NonZeroExit xi exe args
  ExitSuccess -> Right ()


-- | @readCreateProcessWithExitCode@ works exactly like 'readProcessWithExitCode' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the CreateProcess
-- record will be ignored.
--
-- @since 1.2.3.0
readCreateProcessWithExitCodeBS
    :: CreateProcess
    -> BL.ByteString
    -> IO (ExitCode, BL.ByteString, BL.ByteString) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCodeBS cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
      \mb_inh mb_outh mb_errh ph ->
        case (mb_inh, mb_outh, mb_errh) of
          (Just inh, Just outh, Just errh) -> do

            out <- BS.hGetContents outh
            err <- BS.hGetContents errh

            -- fork off threads to start consuming stdout & stderr
            withForkWait  (EX.evaluate $ rnf out) $ \waitOut ->
             withForkWait (EX.evaluate $ rnf err) $ \waitErr -> do

              -- now write any input
              unless (BL.null input) $
                ignoreSigPipe $ BL.hPut inh input
              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
              ignoreSigPipe $ hClose inh

              -- wait on the output
              waitOut
              waitErr

              hClose outh
              hClose errh

            -- wait on the process
            ex <- waitForProcess ph
            return (ex, BL.fromStrict out, BL.fromStrict err)

          (Nothing,_,_) -> error "readCreateProcessWithExitCodeBS: Failed to get a stdin handle."
          (_,Nothing,_) -> error "readCreateProcessWithExitCodeBS: Failed to get a stdout handle."
          (_,_,Nothing) -> error "readCreateProcessWithExitCodeBS: Failed to get a stderr handle."
 where
  ignoreSigPipe :: IO () -> IO ()
  ignoreSigPipe = EX.handle $ \e -> case e of
                                     IOError { ioe_type  = ResourceVanished
                                             , ioe_errno = Just ioe }
                                       | Errno ioe == ePIPE -> return ()
                                     _ -> throwIO e
  -- wrapper so we can get exceptions with the appropriate function name.
  withCreateProcess_
    :: String
    -> CreateProcess
    -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
    -> IO a
  withCreateProcess_ fun c action =
      EX.bracketOnError (createProcess_ fun c) cleanupProcess
                       (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async' body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async') >>= putMVar waitVar
    let wait' = takeMVar waitVar >>= either throwIO return
    restore (body wait') `EX.onException` killThread tid


-- | Execute the given command and collect the stdout, stderr and the exit code.
-- The command is run in a subprocess.
executeOut :: FilePath          -- ^ command as filename, e.g. 'ls'
           -> [String]          -- ^ arguments to the command
           -> Maybe FilePath    -- ^ chdir to this path
           -> IO CapturedProcess
executeOut path args chdir = do
  (exit, out, err) <- readCreateProcessWithExitCodeBS (proc path args){ cwd = chdir } ""
  pure $ CapturedProcess exit out err


execLogged :: (MonadReader AppState m, MonadIO m, MonadThrow m)
           => FilePath         -- ^ thing to execute
           -> [String]         -- ^ args for the thing
           -> Maybe FilePath   -- ^ optionally chdir into this
           -> FilePath         -- ^ log filename (opened in append mode)
           -> Maybe [(String, String)] -- ^ optional environment
           -> m (Either ProcessError ())
execLogged exe args chdir lfile env = do
  AppState { dirs = Dirs {..} } <- ask
  let stdoutLogfile = logsDir </> lfile <> ".stdout.log"
      stderrLogfile = logsDir </> lfile <> ".stderr.log"
  fmap (toProcessError exe args)
    $ liftIO
    $ withCreateProcess
        (proc exe args){ cwd = chdir
                       , env = env
                       , std_in = CreatePipe
                       , std_out = CreatePipe
                       , std_err = CreatePipe
        }
    $ \_ mout merr ph ->
        case (mout, merr) of
          (Just cStdout, Just cStderr) -> do
            withForkWait (tee stdoutLogfile cStdout) $ \waitOut ->
              withForkWait (tee stderrLogfile cStderr) $ \waitErr -> do
                waitOut
                waitErr
            waitForProcess ph
          _ -> fail "Could not acquire out/err handle"

 where
  tee :: FilePath -> Handle -> IO ()
  tee logFile handle' = go
    where
      go = do
        some <- BS.hGetSome handle' 512
        if BS.null some
          then pure ()
          else do
            void $ BS.appendFile logFile some
            void $ BS.hPut stdout some
            go
        

-- | Thin wrapper around `executeFile`.
exec :: FilePath       -- ^ thing to execute
     -> [FilePath]     -- ^ args for the thing
     -> Maybe FilePath   -- ^ optionally chdir into this
     -> Maybe [(String, String)] -- ^ optional environment
     -> IO (Either ProcessError ())
exec exe args chdir env = do
  exit_code <- withCreateProcess
                 (proc exe args) { cwd = chdir, env = env } $ \_ _ _ p ->
                 waitForProcess p
  pure $ toProcessError exe args exit_code


chmod_755 :: MonadIO m => FilePath -> m ()
chmod_755 fp =
  let perm = setOwnerWritable True emptyPermissions
  in liftIO $ setPermissions fp perm
