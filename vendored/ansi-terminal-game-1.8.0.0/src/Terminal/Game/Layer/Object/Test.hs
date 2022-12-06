-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Terminal.Game.Layer.Object.Test where

-- Test (pure) MonadGame* typeclass implementation for testing purposes.

import Terminal.Game.Layer.Object.Interface
import Terminal.Game.Layer.Object.Primitive

import qualified Control.Monad.RWS as S


-----------
-- TYPES --
-----------

data TestEvent = TCleanUpError
               | TQuitGame
               | TSetupDisplay
               | TShutdownDisplay
               | TStartGame
               | TStartEvents
               | TStopEvents
        deriving (Eq, Show)

-- r: ()
-- w: [TestEvents]
-- s: [GTest]
newtype Test a = Test (S.RWS () [TestEvent] GRec a)
               deriving (Functor, Applicative, Monad,
                         S.MonadState GRec,
                         S.MonadWriter [TestEvent])

runTest :: Test a -> GRec -> (a, [TestEvent])
runTest (Test m) es = S.evalRWS m () es


-----------
-- CLASS --
-----------

tconst :: a -> Test a
tconst a = Test $ return a

mockHandle :: InputHandle
mockHandle = InputHandle (error "mock handle keyMvar")
                         (error "mock handle threads")

instance MonadInput Test where
    startEvents _ = S.tell [TStartEvents] >>
                    return mockHandle
    pollEvents _ = S.state getPolled
    stopEvents _ = S.tell [TStopEvents]

instance MonadTimer Test where
    getTime = return 1
    sleepABit _ = return ()

instance MonadException Test where
    cleanUpErr a _ = S.tell [TCleanUpError] >> a
    throwExc e = error . show $ e

instance MonadLogic Test where
    checkQuit _ _ = S.gets isOver

instance MonadDisplay Test where
    setupDisplay = () <$ S.tell [TSetupDisplay]
    clearDisplay = return ()
    displaySize = Test $ S.state getDims
    blitPlane _ _ = return ()
    shutdownDisplay = () <$ S.tell [TShutdownDisplay]
