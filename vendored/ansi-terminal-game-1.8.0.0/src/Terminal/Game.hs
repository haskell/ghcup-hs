-------------------------------------------------------------------------------
-- |
-- Module      :  Terminal.Game
-- Copyright   :  © 2017-2021 Francesco Ariis
-- License     :  GPLv3 (see COPYING file)
--
-- Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
-- Stability   :  provisional
-- Portability :  portable
--
-- Machinery and utilities for 2D terminal games.
--
-- New? Start from 'Game'.
--
-------------------------------------------------------------------------------

-- Basic col-on-black ASCII terminal, operations.
-- Only module to be imported.

module Terminal.Game ( -- * Running
                       TPS,
                       FPS,
                       Event(..),
                       GEnv(..),
                       Game(..),
                       playGame,
                       ATGException(..),

                       -- ** Helpers
                       playGameS,
                       Terminal.Game.displaySize,
                       assertTermDims,
                       errorPress,
                       blankPlaneFull,
                       centerFull,

                       -- * Game logic
                       -- | Some convenient function dealing with
                       -- Timers ('Timed') and 'Animation's.
                       --
                       -- Usage of these is not mandatory: 'Game' is
                       -- parametrised over any state @s@, you are free
                       -- to implement game logic as you prefer.

                       -- ** Timers/Animation

                       -- *** Timers
                       Timed,
                       creaTimer, creaBoolTimer,
                       creaTimerLoop, creaBoolTimerLoop,

                       -- *** Animations
                       Animation,
                       creaAnimation,
                       creaLoopAnimation,

                       -- *** T/A interface
                       tick, ticks, reset, lapse,
                       fetchFrame, isExpired,

                       -- ** Random numbers
                       StdGen,
                       getStdGen, mkStdGen,
                       getRandom, pickRandom,
                       UniformRange,

                       -- * Drawing
                       -- | To get to the gist of drawing, check the
                       -- documentation for '%'.
                       --
                       -- Blitting on screen is double-buffered and diff'd
                       -- (at each frame, only cells with changed character
                       -- will be redrawn).

                       -- ** Plane
                       Plane,
                       Dimensions,
                       Coords,
                       Row, Column,
                       Width, Height,
                       blankPlane,
                       stringPlane,
                       stringPlaneTrans,
                       makeTransparent,
                       makeOpaque,
                       planePaper,
                       planeSize,

                       -- ** Draw
                       Draw,
                       (%), (&), (#),
                       subPlane,
                       mergePlanes,
                       cell, word, box,
                       Color(..), ColorIntensity(..),
                       color, bold, invert,

                       -- *** Alternative origins
                       -- $origins
                       (%^>), (%.<), (%.>),

                       -- *** Text boxes
                       textBox, textBoxLiquid,
                       textBoxHyphen, textBoxHyphenLiquid,
                       Hyphenator,
                       -- | Eurocentric convenience reexports. Check
                       -- "Text.Hyphenation.Language" for more languages.
                       english_GB, english_US, esperanto,
                       french, german_1996, italian, spanish,

                       -- *** Declarative drawing
                       (|||), (===), (***), hcat, vcat,

                       -- * Testing
                       GRec,
                       recordGame,
                       readRecord,
                       testGame,
                       setupGame,
                       narrateGame,

                       -- | A quick and dirty way to have /hot reload/
                       -- (autorestarting your game when source files change)
                       -- is illustrated in @example/MainHotReload.hs@.

                       -- * Cross platform
                       -- $xcompat
                     )
    where

import System.Console.ANSI
import Terminal.Game.Animation
import Terminal.Game.Draw
import Terminal.Game.Layer.Imperative
import Terminal.Game.Layer.Object as O
import Terminal.Game.Plane
import Terminal.Game.Random
import Text.LineBreak

import qualified Control.Monad as CM

-- $origins
-- Placing a plane is sometimes more convenient if the coordinates origin
-- is a corner other than top-left (e.g. “Paste this plane one row from
-- bottom-left corner”). These combinators — meant to be used instead of '%'
-- — allow you to do so. Example:
--
-- @
-- prova :: Plane
-- prova = let rect = box 6 3  \'.\'
--             letters = word "ab"
--         in            rect &
--            (1, 1) %.> letters         -- start from bottom-right
--
--     -- λ> putStr (planePaper prova)
--     -- ......
--     -- ......
--     -- ....ab
-- @

-- $xcompat
-- Good practices for cross-compatibility:
--
-- * choose game dimensions of no more than __24 rows__ and __80 columns__.
--   This ensures compatibility with the trickiest terminals (i.e. Win32
--   console);
--
-- * use __ASCII characters__ only. Again this is for Win32 console
--   compatibility, until
--   [this GHC bug](https://gitlab.haskell.org/ghc/ghc/issues/7593) gets
--   fixed;
--
-- * employ colour sparingly: as some users will play your game in a
--   light-background terminal and some in a dark one, choose only colours
--   that go well with either (blue, red, etc.);
--
-- * some terminals/multiplexers (i.e. tmux) do not make a distinction
--   between vivid/dull; do not base your game mechanics on that
--   difference.

-- | /Usable/ terminal display size (on Win32 console the last line is
-- set aside for input). Throws 'CannotGetDisplaySize' on error.
displaySize :: IO Dimensions
displaySize = O.displaySizeErr

-- | Check if terminal can accomodate 'Dimensions', otherwise throws
-- 'DisplayTooSmall' with a helpful message for the player.
assertTermDims :: Width -> Height -> IO ()
assertTermDims dw dh =
            clearScreen >>
            setCursorPosition 0 0 >>
            displaySizeErr >>= \ads ->
            CM.when (isSmaller ads)
                    (throwExc $ DisplayTooSmall (dw, dh) ads)
    where
          isSmaller :: Dimensions -> Bool
          isSmaller (ww, wh) = ww < dw || wh < dh
