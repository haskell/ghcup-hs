{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Terminal.Game.Layer.Object.Primitive where

import Terminal.Game.Plane

import qualified GHC.Generics as G
import qualified Data.ByteString as BS
import qualified Data.Serialize as Z
import qualified Data.Sequence as S
import qualified Test.QuickCheck as Q

-------------------------------------------------------------------------------
-- Assorted API types

-- | The number of 'Tick's fed each second to the logic function;
-- constant on every machine. /Frames/ per second might be lower
-- (depending on drawing function onerousness, terminal refresh rate,
-- etc.).
type TPS = Integer

-- | The number of frames blit to terminal per second. Frames might be
-- dropped, but game speed will remain constant. Check @balls@
-- (@cabal run -f examples balls@) to see how to display FPS.
-- For obvious reasons (blits would be wasted) @max FPS = TPS@.
type FPS = Integer

-- | An @Event@ is a 'Tick' (time passes) or a 'KeyPress'.
data Event = Tick
           | KeyPress Char
           deriving (Show, Eq, G.Generic)
instance Z.Serialize Event where

instance Q.Arbitrary Event where
  arbitrary = Q.oneof [ pure Tick,
                        KeyPress <$> Q.arbitrary ]

-- | Game environment with current terminal dimensions and current display
-- rate.
data GEnv = GEnv { eTermDims :: Dimensions,
                        -- ^ Current terminal dimensions.
                   eFPS :: FPS
                        -- ^ Current blitting rate.
                       }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- GRec record/replay game typs

-- | Opaque data type with recorded game input, for testing purposes.
data GRec = GRec { aPolled :: S.Seq [Event],
                                -- Seq. of polled events
                   aTermSize :: S.Seq (Maybe Dimensions) }
                                -- Seq. of polled termdims
        deriving (Show, Eq, G.Generic)
instance Z.Serialize GRec where

igrec :: GRec
igrec = GRec S.Empty S.Empty

addDims :: Maybe Dimensions -> GRec -> GRec
addDims mds (GRec p s) = GRec p (mds S.<| s)

getDims :: GRec -> (Maybe Dimensions, GRec)
getDims (GRec p (ds S.:|> d)) = (d, GRec p ds)
getDims _ = error "getDims: empty Seq"
    -- Have to use _ or “non exhaustive patterns” warning

addPolled :: [Event] -> GRec -> GRec
addPolled es (GRec p s) = GRec (es S.<| p) s

getPolled :: GRec -> ([Event], GRec)
getPolled (GRec (ps S.:|> p) d) = (p, GRec ps d)
getPolled _ = error "getEvents: empty Seq"

isOver :: GRec -> Bool
isOver (GRec S.Empty _) = True
isOver _ = False

-- | Reads a file containing a recorded session.
readRecord :: FilePath -> IO GRec
readRecord fp = Z.decode <$> BS.readFile fp >>= \case
                  Left e  -> error $ "readRecord could not decode: " ++
                                     show e
                  Right r -> return r

-- | Convenience function to create a 'GRec' from screen size (constant) plus a list of events. Useful with 'setupGame'.
createGRec :: Dimensions -> [Event] -> GRec
createGRec ds es = let l = length es * 2 in
                   GRec (S.fromList [es])
                        (S.fromList . replicate l $ Just ds)

