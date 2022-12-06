-------------------------------------------------------------------------------
-- Print convenience functions
-- 2017 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

-- Drawing primitives. If not stated otherwise (textbox, etc.), ' ' are
-- assumed to be opaque

module Terminal.Game.Draw (module Terminal.Game.Draw,
                           (F.&)
                          ) where

import Terminal.Game.Plane

import Text.LineBreak

import qualified Data.Function       as F ( (&) )
import qualified Data.List           as L
import qualified System.Console.ANSI as CA


-----------
-- TYPES --
-----------

-- | A drawing function, usually executed with the help of '%'.
type Draw = Plane -> Plane


-----------------
-- COMBINATORS --
-----------------

-- | Pastes one 'Plane' onto another. To be used along with 'F.&'
-- like this:
--
-- @
--  d :: Plane
--  d =          blankPlane 100 100  &
--      (3, 4) % box '_' 3 5         &
--      (a, b) % cell \'A\' '#' bold
-- @
(%) :: Coords -> Plane -> Draw
cds % p1 = \p2 -> pastePlane p1 p2 cds
infixl 4 %

-- | Apply style to plane, e.g.
--
-- > cell 'w' # bold
(#) :: Plane -> Draw -> Plane
p # sf = sf p
infixl 8 #

-- | Shorthand for sequencing 'Plane's, e.g.
--
-- @
--           firstPlane  &
--  (3, 4) '%' secondPlane &
--  (1, 9) '%' thirdPlane
-- @
--
-- is equal to
--
-- @
--  mergePlanes firstPlane [((3,4), secondPlane),
--                          ((1,9), thirdPlane)]
-- @
mergePlanes :: Plane -> [(Coords, Plane)] -> Plane
mergePlanes p cps = L.foldl' addPlane p cps
    where
          addPlane :: Plane -> (Coords, Plane) -> Plane
          addPlane bp (cs, tp) = bp F.& cs % tp

-- | Place two 'Plane's side-by-side, horizontally.
(|||) :: Plane -> Plane -> Plane
(|||) a b = let (wa, ha) = planeSize a
                (wb, hb) = planeSize b
            in mergePlanes (blankPlane (wa + wb) (max ha hb))
                           [((1,1),    a),
                            ((1,wa+1), b)]

-- | Place two 'Plane's side-by-side, vertically.
(===) :: Plane -> Plane -> Plane
(===) a b = let (wa, ha) = planeSize a
                (wb, hb) = planeSize b
            in mergePlanes (blankPlane (max wa wb) (ha + hb))
                           [((1,1),    a),
                            ((ha+1,1), b)]

-- | @a *** b@ blits @b@ in the centre of @a@.
(***) :: Plane -> Plane -> Plane
(***) a b = let (aw, ah) = planeSize a
                (bw, bh) = planeSize b
                r = quot (ah - bh) 2 + 1
                c = quot (aw - bw) 2 + 1
            in           a F.&
                (r, c) % b


-- | Place a list of 'Plane's side-by-side, horizontally. @error@s on
-- empty list.
hcat :: [Plane] -> Plane
hcat [] = blankPlane 1 1 # makeTransparent ' '
hcat ps = L.foldl1' (|||) ps

-- | Place a list of 'Plane's side-by-side, vertically. @error@s on
-- empty list.
vcat :: [Plane] -> Plane
vcat [] = blankPlane 1 1 # makeTransparent ' '
vcat ps = L.foldl1' (===) ps

infixl 6 |||, ===, ***


------------
-- STYLES --
------------

-- | Set foreground color.
color :: CA.Color -> CA.ColorIntensity -> Plane -> Plane
color c i p = mapPlane (colorCell c i) p

-- | Apply bold style to 'Plane'.
bold :: Plane -> Plane
bold p = mapPlane boldCell p

-- | Swap foreground and background colours of 'Plane'.
invert :: Plane -> Plane
invert p = mapPlane reverseCell p



-------------
-- DRAWING --
-------------

-- | A box of dimensions @w h@.
box :: Width -> Height -> Char -> Plane
box w h chr = seqCellsDim w h cells
    where
          cells = [((r, c), chr) | r <- [1..h], c <- [1..w]]

-- | A 1×1 @Plane@.
cell :: Char -> Plane
cell ch = box 1 1 ch

-- | @1xn@ 'Plane' with a word in it. If you need to import multiline
-- ASCII art, check 'stringPlane' and 'stringPlaneTrans'.
word :: String -> Plane
word w = seqCellsDim (L.genericLength w) 1 cells
    where
          cells = zip (zip (repeat 1) [1..]) w

-- opaque :: Plane -> Plane
-- opaque p = pastePlane p (box ' ' White w h) (1, 1)
--     where
--           (w, h) = pSize p

-- | A text-box. Assumes @' '@s are transparent.
textBox :: Width -> Height -> String -> Plane
textBox w h cs = frameTrans w h (textBoxLiquid w cs)

-- | Like 'textBox', but tall enough to fit @String@.
textBoxLiquid :: Width -> String -> Plane
textBoxLiquid w cs = textBoxGeneralLiquid Nothing w cs

-- | As 'textBox', but hypenated. Example:
--
-- @
-- (normal textbox)                        (hyphenated textbox)
-- Rimasi un po’ a meditare nel buio       Rimasi un po’ a meditare nel buio
-- velato appena dal barlume azzurrino     velato appena dal barlume azzurrino
-- del fornello a gas, su cui              del fornello a gas, su cui sobbol-
-- sobbolliva quieta la pentola.           liva quieta la pentola.
-- @
--
-- Notice how in the right box /sobbolliva/ is broken in two. This
-- can be useful and aesthetically pleasing when textboxes are narrow.
textBoxHyphen :: Hyphenator -> Width -> Height -> String -> Plane
textBoxHyphen hp w h cs = frameTrans w h (textBoxHyphenLiquid hp w cs)

-- | As 'textBoxLiquid', but hypenated.
textBoxHyphenLiquid :: Hyphenator -> Width -> String -> Plane
textBoxHyphenLiquid h w cs = textBoxGeneralLiquid (Just h) w cs

textBoxGeneralLiquid :: Maybe Hyphenator -> Width -> String -> Plane
textBoxGeneralLiquid mh w cs = transparent
    where
          -- hypenathion
          bf  = BreakFormat (fromIntegral w) 4 '-' mh
          hcs = breakStringLn bf cs
          h   = L.genericLength hcs

          f :: [String] -> [(Coords, Char)]
          f css = concatMap (uncurry rf) (zip [1..] css)
              where rf :: Int -> String -> [(Coords, Char)]
                    rf cr ln = zip (zip (repeat cr) [1..]) ln

          out         = seqCellsDim w h (f hcs)
          transparent = makeTransparent ' ' out


----------------------------
-- ADDITIONAL COMBINATORS --
----------------------------

-- Coords as if origin were @ bottom-right
recipCoords :: Coords -> Plane -> Plane -> Coords
recipCoords (r, c) p p1 =
            let (pw, ph) = planeSize p
                (p1w, p1h) = planeSize p1
                r' = ph-p1h-r+2
                c' = pw-p1w-c+2
            in (r', c')

-- | Pastes a plane onto another (origin: top-right).
(%^>) :: Coords -> Plane -> Draw
(r, c) %^> p1 = \p ->
            let (_, c') = recipCoords (r, c) p p1
            in p F.& (r, c') % p1

-- | Pastes a plane onto another (origin: bottom-left).
(%.<) :: Coords -> Plane -> Draw
(r, c) %.< p1 = \p ->
            let (r', _) = recipCoords (r, c) p p1
            in p F.& (r', c) % p1

-- | Pastes a plane onto another (origin: bottom-right).
(%.>) :: Coords -> Plane -> Draw
cs %.> p1 = \p ->
            let (r', c') = recipCoords cs p p1
            in p F.& (r', c') % p1

infixl 4 %^>
infixl 4 %.<
infixl 4 %.>


-----------------
-- ANCILLARIES --
-----------------

seqCellsDim :: Width -> Height -> [(Coords, Char)] -> Plane
seqCellsDim w h cells = seqCells (blankPlane w h) cells

seqCells :: Plane -> [(Coords, Char)] -> Plane
seqCells p cells = updatePlane p (map f cells)
    where
          f (cds, chr) = (cds, creaCell chr)

-- paste plane on a blank one, and make ' ' transparent
frameTrans :: Width -> Height -> Plane -> Plane
frameTrans w h p = let bt = makeTransparent ' ' (blankPlane w h)
                   in bt F.& (1, 1) % p

