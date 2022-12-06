module Terminal.Game.RandomSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Terminal.Game.Random


spec :: Spec
spec = do

  describe "pickRandom" $ do
    prop "picks items at random from a list" $
      \i -> let g = mkStdGen i
            in fst (pickRandom ['a', 'b'] g) /= 'c'
    prop "does not exclude any item" $
      \i -> let g = mkStdGen i
                rf tg = pickRandom [1,2] tg
                rs = iterate (\(_, lg') -> rf lg')  (rf g)
                ts = take 100 rs
            in sum (map fst ts) /= length ts
