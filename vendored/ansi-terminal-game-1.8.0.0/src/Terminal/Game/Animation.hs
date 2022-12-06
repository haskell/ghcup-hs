-------------------------------------------------------------------------------
-- Animation
-- 2018 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Terminal.Game.Animation (module Terminal.Game.Animation,
                                module T
                               ) where

import Terminal.Game.Plane

import Control.Timer.Tick as T

-- import Data.Serialize

-- import qualified Data.ByteString as BS
-- import qualified Data.Bifunctor as BF

-- | An @Animation@ is a series of timed time-separated 'Plane's.
type Animation = T.Timed Plane

-- | Creates an 'Animation'.
creaAnimation :: [(Integer, Plane)] -> Animation
creaAnimation ips = creaTimedRes (Times 1 Elapse) ips

-- | Creates a looped 'Animation'.
creaLoopAnimation :: [(Integer, Plane)] -> Animation
creaLoopAnimation ips = creaTimedRes AlwaysLoop ips
