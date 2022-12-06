-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Terminal.Game.Layer.Object.GameIO where

import qualified Control.Monad.Catch          as MC
import qualified Control.Monad.Trans          as T


newtype GameIO a = GameIO { runGIO :: IO a }
                 deriving (Functor, Applicative, Monad,
                           T.MonadIO,
                           MC.MonadThrow, MC.MonadCatch, MC.MonadMask)


