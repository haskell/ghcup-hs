{-# LANGUAGE DeriveGeneric #-}

-------------------------------------------------------------------------------
-- Screen datatypes and functions
-- 2017 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

-- a canvas where to draw our stuff

module Terminal.Game.Plane where

import Terminal.Game.Character

import qualified Data.Array          as A
import qualified Data.Bifunctor      as B
import qualified Data.List.Split     as LS
import qualified Data.Tuple          as T
import qualified GHC.Generics        as G
import qualified System.Console.ANSI as CA


----------------
-- DATA TYPES --
----------------

-- | 'Row's and 'Column's are 1-based (top-left position is @1 1@).
type Coords = (Row, Column)
type Row    = Int
type Column = Int

-- | Size of a surface in 'Row's and 'Column's.
type Dimensions = (Width, Height)

-- | Expressed in 'Column's.
type Width  = Int
-- | Expressed in 'Row's.
type Height = Int

type Bold     = Bool
type Reversed = Bool

-- can be an ASCIIChar or a special, transparent character
data Cell = CellChar Char Bold
                     Reversed (Maybe (CA.Color, CA.ColorIntensity))
          | Transparent
          deriving (Show, Eq, Ord, G.Generic)
        -- I found no meaningful speed improvements by making this
        -- only w/ 1 constructor.

-- | A two-dimensional surface (Row, Column) where to blit stuff.
newtype Plane = Plane { fromPlane :: A.Array Coords Cell }
              deriving (Show, Eq, G.Generic)
        -- Could this be made into an UArray? Nope, since UArray is
        -- only instanced on Words, Int, Chars, etc.

-------------------------------------------------------------------------------
-- Plane interface (abstracting Array)

listPlane :: Coords -> [Cell] -> Plane
listPlane (r, c) cs = Plane $ A.listArray ((1,1), (r, c)) cs

-- | Dimensions or a plane.
planeSize :: Plane -> Dimensions
planeSize p = T.swap . snd $ A.bounds (fromPlane p)

assocsPlane :: Plane -> [(Coords, Cell)]
assocsPlane p = A.assocs (fromPlane p)

elemsPlane :: Plane -> [Cell]
elemsPlane p = A.elems (fromPlane p)

-- Array.//
updatePlane :: Plane -> [(Coords, Cell)] -> Plane
updatePlane (Plane a) kcs = Plane $ a A.// kcs

-- faux map
mapPlane :: (Cell -> Cell) -> Plane -> Plane
mapPlane f (Plane a) = Plane $ fmap f a


----------
-- CREA --
----------

creaCell :: Char -> Cell
creaCell ch = CellChar chm False False Nothing
    where
          chm = win32SafeChar ch

colorCell :: CA.Color -> CA.ColorIntensity -> Cell -> Cell
colorCell k i (CellChar c b r _) = CellChar c b r (Just (k, i))
colorCell _ _ Transparent        = Transparent

boldCell :: Cell -> Cell
boldCell (CellChar c _ r k) = CellChar c True r k
boldCell Transparent        = Transparent

reverseCell :: Cell -> Cell
reverseCell (CellChar c b _ k) = CellChar c b True k
reverseCell Transparent        = Transparent

-- | Creates 'Plane' from 'String', good way to import ASCII
-- art/diagrams. @error@s on empty string.
stringPlane :: String -> Plane
stringPlane t = stringPlaneGeneric Nothing t

-- | Same as 'stringPlane', but with transparent 'Char'.
-- @error@s on empty string.
stringPlaneTrans :: Char -> String -> Plane
stringPlaneTrans c t = stringPlaneGeneric (Just c) t

-- | Creates an empty, opaque 'Plane'.
blankPlane :: Width -> Height -> Plane
blankPlane w h = listPlane (h, w) (repeat $ creaCell ' ')

-- | Adds transparency to a plane, matching a given character
makeTransparent :: Char -> Plane -> Plane
makeTransparent tc p = mapPlane f p
    where
          f cl | cellChar cl == tc = Transparent
               | otherwise         = cl

-- | Changes every transparent cell in the 'Plane' to an opaque @' '@
-- character.
makeOpaque :: Plane -> Plane
makeOpaque p = let (w, h) = planeSize p
               in pastePlane p (blankPlane w h) (1, 1)



-----------
-- SLICE --
-----------

-- | Paste one plane over the other at a certain position (p1 gets over p2).
pastePlane :: Plane -> Plane -> Coords -> Plane
pastePlane p1 p2 (r, c)
            | r > h2 || c > w2 = p2
            | otherwise =
                let ks = assocsPlane p1
                    fs = filter (\x -> solid x && inside x) ks
                    ts = fmap (B.first trasl) fs
                in updatePlane p2 ts
    where
          trasl :: Coords -> Coords
          trasl (wr, wc) = (wr + r - 1, wc + c - 1)

          -- inside new position, cheaper than first mapping and then
          -- filtering.
          inside (wcs, _) =
                let (r1', c1') = trasl wcs
                in r1' >= 1 && r1' <= h2 &&
                   c1' >= 1 && c1' <= w2

          solid (_, Transparent) = False
          solid _                = True

          (w2, h2)  = planeSize p2

-- | Cut out a plane by top-left and bottom-right coordinates.
subPlane :: Plane -> Coords -> Coords -> Plane
subPlane p (r1, c1) (r2, c2)
        | r1 > r2 || c1 > c2 = err (r1, c1) (r2, c2)
        | otherwise          =
            let cs       = assocsPlane p
                fs       = filter f cs
                (pw, ph) = planeSize p
                (w, h)   = (min pw (c2-c1+1), min ph (r2-r1+1))
            in listPlane (h, w) (map snd fs)
    where
          f ((rw, cw), _) = rw >= r1 && rw <= r2 &&
                            cw >= c1 && cw <= c2

          err p1 p2 = error ("subPlane: top-left point " ++ show p1 ++
                             " > bottom-right point " ++ show p2 ++ ".")

-------------
-- INQUIRE --
-------------

cellChar :: Cell -> Char
cellChar (CellChar c _ _ _) = c
cellChar Transparent        = ' '

cellColor :: Cell -> Maybe (CA.Color, CA.ColorIntensity)
cellColor (CellChar _ _ _ k) = k
cellColor Transparent        = Nothing

isBold :: Cell -> Bool
isBold (CellChar _ b _ _) = b
isBold _                  = False

isReversed :: Cell -> Bool
isReversed (CellChar _ _ r _) = r
isReversed _                  = False

-- | A String (@\n@ divided and ended) representing the 'Plane'. Useful
-- for debugging/testing purposes.
planePaper :: Plane -> String
planePaper p = unlines . LS.chunksOf w . map cellChar $ elemsPlane p
    where
          w :: Int
          w = fromIntegral . fst . planeSize $ p

-----------------
-- ANCILLARIES --
-----------------

stringPlaneGeneric :: Maybe Char -> String -> Plane
stringPlaneGeneric _ "" = makeTransparent ' ' (blankPlane 1 1)
stringPlaneGeneric mc t = vitrous
    where
          lined = lines t

          h :: Int
          h = length lined

          w :: Int
          w = maximum (map length lined)

          pad :: Int -> String -> String
          pad mw tl = take mw (tl ++ repeat ' ')

          padded :: [String]
          padded = map (pad w) lined

          celled :: [Cell]
          celled = map creaCell . concat $ padded

          plane :: Plane
          plane = listPlane (h, w) celled

          vitrous :: Plane
          vitrous = case mc of
                      Just c  -> makeTransparent c plane
                      Nothing -> plane

