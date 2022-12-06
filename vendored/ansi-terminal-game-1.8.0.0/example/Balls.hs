module Balls where

-- library module for `balls`

import Terminal.Game

import qualified Data.Bool as B
import qualified Data.Ix as I
import qualified Data.Maybe as M
import qualified Data.Tuple as T

{-
   There are three things I will showcase in this example:

   1. ** How you can display current FPS. **
      This is done using `Game` to create your game rather than
      `simpleGame`. `Game` is a bit more complex but you gain
      additional infos to manipulate/blit, like FPS.

   2. ** How your game can gracefully handle screen resize. **
      Notice how if you resize the terminal, balls will still
      fill the entire screen. This is again possible using `Game`
      and the information passed via GameEnv (in this case, terminal
      dimensions).

   3. ** That — while FPS can change  — game speed does not. **
      Check the timer: even when screen is crowded and frames are
      dropped, it is not slowed down.


   This game runs at 60 FPS, you will almost surely never need such
   a high TPS! 15–20 is more than enough in most cases.
-}

-------------------------------------------------------------------------------
-- Ball

data Ball = Ball { pChar :: Plane,
                   pSpeed :: Timed Bool,
                   pDir :: Coords,
                   pPos :: Coords }

-- change direction is necessary, then and move
modPar :: Dimensions -> Ball -> Maybe Ball
modPar ds b@(Ball _ _ d _) =
            -- tick the ball and check it is time to move
            let b' = tickBall b in
            if not (fetchFrame . pSpeed $ b')
              then Just b' -- no time to move for you
            else

            -- check all popssible directions
            let pd = [d, togR d, togC d, togB d]
                bs  = map (\ld -> b' { pDir = ld })  pd
                bs' = filter (isIn ds) $ map modPos bs in

            -- returns a moved ball nor nothing to mark it “to eliminate”
            case bs' of
              [] -> Nothing
              (cp:_) -> Just cp
    where
          togR (wr, wc) = (-wr,  wc)
          togC (wr, wc) = ( wr, -wc)
          togB (wr, wc) = (-wr, -wc)

tickBall :: Ball -> Ball
tickBall b = b { pSpeed = tick (pSpeed b) }

modPos :: Ball -> Ball
modPos (Ball p t d@(dr, dc) (r, c)) = Ball p t d (r+dr, c+dc)

isIn :: Dimensions -> Ball -> Bool
isIn (w, h) (Ball p _ _ (pr, pc)) =
            let (pw, ph) = planeSize p
            in pr >= 1 &&
               pr+ph-1 <= h &&
               pc >= 1 &&
               pc+pw-1 <= w

dpart :: Ball -> (Coords, Plane)
dpart (Ball p _ _ cs) = (cs, p)

genBall :: StdGen -> Dimensions -> (Ball, StdGen)
genBall g ds =
                let (c, g1) = pickRandom [minBound..] g
                    (s, g2) = getRandom (1, 3) g1
                    (v, g3) = pickRandom dirs g2
                    (p, g4) = ranIx ((1,1), T.swap ds) g3
                    b = Ball (cell 'o' # color c Vivid)
                             (creaBoolTimerLoop s) v p
                in (b, g4)
        where
              dirs = [(1, 1), (1, -1), (-1, 1), (-1, -1)]

              -- tuples instances are yet to be added to `random`
              -- as nov 21; this will do meanwhile.
              ranIx :: I.Ix a => (a, a) -> StdGen -> (a, StdGen)
              ranIx r wg = pickRandom (I.range r) wg

-------------------------------------------------------------------------------
-- Timer

type Timer = (Timed Bool, Integer)

ctimer :: TPS -> Timer
ctimer tps = (creaBoolTimerLoop tps, 0)

ltimer :: Timer -> Timer
ltimer (t, i) = let t' = tick t
                    k = B.bool 0 1 (fetchFrame t')
                in (t', i+k)

dtimer :: Timer -> Plane
dtimer (_, i) = word . show $ i

-------------------------------------------------------------------------------
-- Game

data GState = GState { gen :: StdGen,
                       quit :: Bool,
                       timer :: Timer,
                       balls :: [Ball],
                       bslow :: Bool }
            -- pSlow is not used in game, it is there just
            -- for the test suite

fireworks :: StdGen -> Game GState
fireworks g = Game tps istate lfun dfun qfun
    where
          tps = 60

          istate :: GState
          istate = GState g False (ctimer tps) [] False

-------------------------------------------------------------------------------
-- Logic

lfun :: GEnv -> GState -> Event -> GState
lfun e s (KeyPress 's') =
            let g = gen s
                ds = eTermDims e
                (b, g1) = genBall g ds
            in s { gen = g1,
                   balls = b : balls s }
lfun _ s (KeyPress 'q') = s { quit = True }
lfun _ s (KeyPress _)   = s
lfun r s Tick           =
            let ds = eTermDims r

                ps = balls s
                ps' = M.mapMaybe (modPar ds) ps

                bs = eFPS r < 30
            in s { timer = ltimer (timer s),
                   balls = filter (isIn ds)  ps',
                   bslow = bs }

qfun :: GState -> Bool
qfun s = quit s

-------------------------------------------------------------------------------
-- Draw

dfun :: GEnv -> GState -> Plane
dfun r s =             mergePlanes
                         (uncurry blankPlane ds)
                         (map dpart $ balls s) &
            (1, 2) %^> tui # trans &
            (1, 2) %.< inst # trans # bold
    where
          ds = eTermDims r
          tm = timer s

          tui :: Plane
          tui = let fps = eFPS r
                    np = length $ balls s

                    l1 = word "FPS: " ||| word (show fps)
                    l2 = word "Timer: " ||| dtimer tm
                    l3 = word ("Balls: " ++ show np)
                    l4 = word ("Term. dims.: " ++ show ds)
                in vcat [l1, l2, l3, l4]

          inst :: Plane
          inst = word "Press (s) to spawn" ===
                 word "Press (q) to quit"

          trans :: Draw
          trans = makeTransparent ' '
