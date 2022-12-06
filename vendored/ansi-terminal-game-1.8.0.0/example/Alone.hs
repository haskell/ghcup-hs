module Alone where

-- Alone in a room, game definition (logic & draw)
-- run with: cabal new-run -f examples alone

import Terminal.Game

import qualified Data.Tuple as T

-- game specification
aloneInARoom :: Game MyState
aloneInARoom = Game 13                       -- ticks per second
                    (MyState (10, 10)
                             Stop False)     -- init state
                    (\_ s e -> logicFun s e) -- logic function
                    (\r s -> centerFull r $
                               drawFun s)    -- draw function
                    gsQuit                   -- quit function

sizeCheck :: IO ()
sizeCheck = let (w, h) = T.swap . snd $ boundaries
            in assertTermDims w h

-------------------------------------------------------------------------------
-- Types

data MyState = MyState { gsCoord :: Coords,
                         gsMove  :: Move,
                         gsQuit  :: Bool }
             deriving (Show, Eq)

data Move = N | S | E | W | Stop
          deriving (Show, Eq)

boundaries :: (Coords, Coords)
boundaries = ((1, 1), (24, 80))

-------------------------------------------------------------------------------
-- Logic

logicFun :: MyState -> Event -> MyState
logicFun gs (KeyPress 'q') = gs { gsQuit = True }
logicFun gs Tick           = gs { gsCoord = pos (gsMove gs) (gsCoord gs) }
logicFun gs (KeyPress c)   = gs { gsMove = move (gsMove gs) c }

-- SCI movement
move :: Move -> Char -> Move
move N 'w' = Stop
move S 's' = Stop
move W 'a' = Stop
move E 'd' = Stop
move _ 'w' = N
move _ 's' = S
move _ 'a' = W
move _ 'd' = E
move m _   = m

pos :: Move -> (Width, Height) -> (Width, Height)
pos m oldcs | oob newcs = oldcs
            | otherwise = newcs
    where
          newcs = new m oldcs

          new Stop cs = cs
          new N    (r, c) = (r-1, c  )
          new S    (r, c) = (r+1, c  )
          new E    (r, c) = (r  , c+1)
          new W    (r, c) = (r  , c-1)

          ((lr, lc), (hr, hc)) = boundaries
          oob (r, c) = r <= lr || c <= lc ||
                       r >= hr || c >= hc

-------------------------------------------------------------------------------
-- Draw

drawFun :: MyState -> Plane
drawFun (MyState (r, c) _ _) =
                           blankPlane mw     mh            &
                (1, 1)   % box mw mh '-'                   &
                (2, 2)   % box (mw-2) (mh-2) ' '           &
                (15, 20) % textBox 10 4
                                   "Tap WASD to move, tap again to stop." &
                (20, 60) % textBox 8 10
                                   "Press Q to quit." # color Blue Vivid &
                (r, c) % cell '@' # invert
    where
          mh :: Height
          mw :: Width
          (mh, mw) = snd boundaries
