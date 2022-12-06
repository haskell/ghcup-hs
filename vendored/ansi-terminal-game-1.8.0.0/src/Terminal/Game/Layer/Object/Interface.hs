-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Terminal.Game.Layer.Object.Interface where

import Terminal.Game.Plane
import Terminal.Game.Layer.Object.Primitive

import qualified Control.Concurrent as CC
import qualified Control.Monad.Catch as MC

-------------------------------------------------------------------------------
-- mtl interface for game

type MonadGameIO m = (MonadInput m, MonadTimer m,
                      MonadException m, MonadLogic m,
                      MonadDisplay m)

data InputHandle = InputHandle
            { ihKeyMVar     :: CC.MVar [Event],
              ihOpenThreads :: [CC.ThreadId] }

class Monad m => MonadInput m where
    startEvents :: TPS -> m InputHandle
    pollEvents  :: CC.MVar [Event] -> m [Event]
    stopEvents :: [CC.ThreadId] -> m ()

class Monad m => MonadTimer m where
    getTime :: m Integer     -- to nanoseconds
    sleepABit :: TPS -> m () -- Given TPS, sleep a fracion of a single
                             -- Tick.

-- if a fails, do b (useful for cleaning up)
class Monad m => MonadException m where
    cleanUpErr :: m a -> m b -> m a
    throwExc :: ATGException -> m a

class Monad m => MonadLogic m where
    -- decide whether it's time to quit
    checkQuit :: (s -> Bool) -> s -> m Bool

class Monad m => MonadDisplay m where
    setupDisplay :: m ()
    clearDisplay :: m ()
    displaySize :: m (Maybe Dimensions)
    blitPlane :: Maybe Plane -> Plane -> m ()
    shutdownDisplay :: m ()

displaySizeErr :: (MonadDisplay m, MonadException m) => m Dimensions
displaySizeErr = displaySize >>= \case
                   Nothing -> throwExc CannotGetDisplaySize
                   Just d -> return d

-------------------------------------------------------------------------------
-- Error handling

-- | @ATGException@s are thrown synchronously for easier catching.
data ATGException = CannotGetDisplaySize
                  | DisplayTooSmall Dimensions Dimensions
                        -- ^ Required and actual dimensions.

instance Show ATGException where
    show CannotGetDisplaySize = "CannotGetDisplaySize"
    show (DisplayTooSmall (sw, sh) tds) =
      let colS ww = ww < sw
          rowS wh = wh < sh

          smallMsg :: Dimensions -> String
          smallMsg (ww, wh) =
                let cm = show ww ++ " columns"
                    rm = show wh ++ " rows"
                    em | colS ww && rowS wh = cm ++ " and " ++ rm
                       | colS ww = cm
                       | rowS wh = rm
                       | otherwise = "smallMsg: passed correct term size!"
                in
                  "This games requires a display of " ++ show sw ++
                  " columns and " ++ show sh ++ " rows.\n" ++
                  "Yours only has " ++ em ++ "!\n\n" ++
                  "Please resize your terminal and restart the game.\n"
      in "DisplayTooSmall.\n" ++ smallMsg tds

instance MC.Exception ATGException where
