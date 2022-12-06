-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

module Terminal.Game.Layer.Object ( module Export ) where

import Terminal.Game.Layer.Object.Interface as Export
import Terminal.Game.Layer.Object.GameIO    as Export
import Terminal.Game.Layer.Object.Narrate   as Export
import Terminal.Game.Layer.Object.Primitive as Export
import Terminal.Game.Layer.Object.Record    as Export
import Terminal.Game.Layer.Object.Test      as Export

-- DESIGN NOTES --

-- The classes are described in 'Interface'.

-- The implemented monads are four:
--     - GameIO (via MonadIO): playing the game
--     - Test: testing the game in a pure manner
--     - Record: playing the game and record the Events in a file
--     - Replay: replay a game using a set of Events.
--
-- The last two monads (Record/Replay) take advantage of "overlapping
-- instances". Instead of reimplementing most of what happens in MonadIO,
-- they'll just touch the classes from interface which behaviour they
-- will modify; being more specific, they will be chosen instead of plain
-- IO.
