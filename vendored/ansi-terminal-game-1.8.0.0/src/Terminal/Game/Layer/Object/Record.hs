{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Terminal.Game.Layer.Object.Record where

-- Record Monad, for when I need to play the game and record Events
-- (keypresses, ticks, screen size, FPS) to a file.

import Terminal.Game.Layer.Object.Interface
import Terminal.Game.Layer.Object.Primitive
import Terminal.Game.Layer.Object.IO ()

import qualified Control.Concurrent   as CC
import qualified Control.Monad.Catch  as MC
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Trans  as T -- MonadIO
import qualified Data.ByteString      as BS
import qualified Data.Serialize       as S

-- record the key pressed in a game session

newtype Record a = Record (R.ReaderT (CC.MVar GRec) IO a)
                deriving (Functor, Applicative, Monad,
                          T.MonadIO, R.MonadReader (CC.MVar GRec),
                          MC.MonadThrow, MC.MonadCatch, MC.MonadMask)

-- Lifts IO interface, records where necessary
instance MonadInput Record where
    startEvents tps = T.liftIO (startEvents tps)
    pollEvents ve = T.liftIO (pollEvents ve) >>= \es ->
                    modMRec addPolled es
    stopEvents ts = T.liftIO (stopEvents ts)

instance MonadDisplay Record where
    setupDisplay = T.liftIO setupDisplay
    clearDisplay = T.liftIO clearDisplay
    displaySize = T.liftIO displaySize >>= \ds ->
                  modMRec addDims ds
    blitPlane mp p = T.liftIO (blitPlane mp p)
    shutdownDisplay = T.liftIO shutdownDisplay

-- logs and passes the value on
modMRec :: (a -> GRec -> GRec) -> a -> Record a
modMRec f a = R.ask >>= \mv ->
              let fmv = CC.modifyMVar_ mv (return . f a) in
              T.liftIO fmv >>
              return a

runRecord :: Record a -> CC.MVar GRec -> IO a
runRecord (Record r) me = R.runReaderT r me

writeRec :: FilePath -> CC.MVar GRec -> IO ()
writeRec fp vr = CC.readMVar vr                >>= \k ->
                 BS.writeFile fp (S.encode k)
