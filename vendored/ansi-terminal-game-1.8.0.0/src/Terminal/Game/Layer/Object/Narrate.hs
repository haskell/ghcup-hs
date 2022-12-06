{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Terminal.Game.Layer.Object.Narrate where

-- Narrate Monad, replay on screen from a GRec

import Terminal.Game.Layer.Object.Interface
import Terminal.Game.Layer.Object.Primitive
import Terminal.Game.Layer.Object.IO () -- MonadIo

import qualified Control.Monad.Catch as MC
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T


newtype Narrate a = Narrate (S.StateT GRec IO a)
                deriving (Functor, Applicative, Monad,
                          T.MonadIO, S.MonadState GRec,
                          MC.MonadThrow, MC.MonadCatch, MC.MonadMask)

instance MonadInput Narrate where
    startEvents fps = T.liftIO $ startEvents fps
    pollEvents _ = S.state getPolled
    stopEvents ts = T.liftIO $ stopEvents ts

instance MonadLogic Narrate where
    checkQuit _ _ = S.gets isOver

runReplay :: Narrate a -> GRec -> IO a
runReplay (Narrate s) k = S.evalStateT s k
