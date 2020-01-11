{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Utils.File where

import           GHCup.Utils.Dirs
import           GHCup.Utils.Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Data.ByteString
import           Data.ByteString.Unsafe         ( unsafeUseAsCStringLen )
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           GHC.Foreign                    ( peekCStringLen )
import           GHC.IO.Encoding                ( getLocaleEncoding )
import           GHC.IO.Exception
import           HPath
import           HPath.IO
import           Optics
import           Streamly
import           Streamly.External.ByteString
import           Streamly.External.ByteString.Lazy
import           System.IO
import           System.Posix.Directory.ByteString
import           System.Posix.FD               as FD
import           System.Posix.FilePath   hiding ( (</>) )
import           System.Posix.Foreign           ( oExcl )
import "unix"    System.Posix.IO.ByteString
                                         hiding ( openFd )
import           System.Posix.Process           ( ProcessStatus(..) )
import           System.Posix.Types


import qualified System.Posix.Process.ByteString
                                               as SPPB
import           Streamly.External.Posix.DirStream
import qualified Streamly.Internal.Memory.ArrayStream
                                               as AS
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.Data.Unfold as SU
import qualified Streamly.Prelude              as S
import qualified Data.ByteString.Lazy          as L


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


readFd :: Fd -> IO L.ByteString
readFd fd = do
  handle' <- fdToHandle fd
  fromChunksIO $ (S.unfold (SU.finallyIO hClose FH.readChunks) handle')


-- | Read the lines of a file into a stream. The stream holds
-- a file handle as a resource and will close it once the stream
-- terminates (either through exception or because it's drained).
readFileLines :: Path b -> IO (SerialT IO ByteString)
readFileLines p = do
  stream <- readFileStream p
  pure
    . (fmap fromArray)
    . AS.splitOn (fromIntegral $ ord '\n')
    . (fmap toArray)
    $ stream


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
executeOut :: Path b          -- ^ command as filename, e.g. 'ls'
           -> [ByteString]      -- ^ arguments to the command
           -> Maybe (Path Abs)  -- ^ chdir to this path
           -> IO CapturedProcess
executeOut path args chdir = captureOutStreams $ do
  maybe (pure ()) (changeWorkingDirectory . toFilePath) chdir
  SPPB.executeFile (toFilePath path) True args Nothing


execLogged :: ByteString       -- ^ thing to execute
           -> Bool             -- ^ whether to search PATH for the thing
           -> [ByteString]     -- ^ args for the thing
           -> Path Rel         -- ^ log filename
           -> Maybe (Path Abs) -- ^ optionally chdir into this
           -> Maybe [(ByteString, ByteString)] -- ^ optional environment
           -> IO (Either ProcessError ())
execLogged exe spath args lfile chdir env = do
  ldir <- ghcupLogsDir
  let logfile = ldir </> lfile
  bracket (createFile (toFilePath logfile) newFilePerms) closeFd action
 where
  action fd = do
    pid <- SPPB.forkProcess $ do
      -- dup stdout
      void $ dupTo fd stdOutput

      -- dup stderr
      void $ dupTo fd stdError

      -- execute the action
      maybe (pure ()) (changeWorkingDirectory . toFilePath) chdir
      SPPB.executeFile exe spath args env


    SPPB.getProcessStatus True True pid >>= \case
      i@(Just (SPPB.Exited _)) -> pure $ toProcessError exe args i
      i                         -> pure $ toProcessError exe args i


-- | Capture the stdout and stderr of the given action, which
-- is run in a subprocess. Stdin is closed. You might want to
-- 'race' this to make sure it terminates.
captureOutStreams :: IO a
                     -- ^ the action to execute in a subprocess
                  -> IO CapturedProcess
captureOutStreams action =
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
        void $ action

      -- close everything we don't need
      closeFd childStdoutWrite
      closeFd childStderrWrite

      SPPB.getProcessStatus True True pid >>= \case
        -- readFd will take care of closing the fd
        Just (SPPB.Exited es) -> do
          stdout' <- L.toStrict <$> readFd parentStdoutRead
          stderr' <- L.toStrict <$> readFd parentStderrRead
          pure $ CapturedProcess { _exitCode = es
                                 , _stdOut   = stdout'
                                 , _stdErr   = stderr'
                                 }
        _ -> throwIO $ userError $ ("No such PID " ++ show pid)

 where
  actionWithPipes a =
    createPipe >>= \(p1, p2) -> (flip finally) (cleanup [p1, p2]) $ a (p1, p2)
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


-- | Convert the String to a ByteString with the current
-- system encoding.
unsafePathToString :: Path b -> IO FilePath
unsafePathToString p = do
  enc <- getLocaleEncoding
  unsafeUseAsCStringLen (toFilePath p) (peekCStringLen enc)


-- | Search for a file in the search paths.
--
-- Catches `PermissionDenied` and `NoSuchThing` and returns `Nothing`.
searchPath :: [Path Abs] -> Path Rel -> IO (Maybe (Path Abs))
searchPath paths needle = go paths
 where
  go [] = pure Nothing
  go (x : xs) =
    hideErrorDefM PermissionDenied (go xs)
      $ hideErrorDefM NoSuchThing (go xs)
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
