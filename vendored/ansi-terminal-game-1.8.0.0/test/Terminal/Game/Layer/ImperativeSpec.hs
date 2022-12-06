module Terminal.Game.Layer.ImperativeSpec where

import Terminal.Game.Layer.Imperative
import Terminal.Game.Layer.Object
import Terminal.Game.Random
import Alone
import Balls

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Test.QuickCheck as Q

spec :: Spec
spec = do

  describe "runGame" $ do
    let nd = error "<not-defined>"
        s :: (Integer, Bool, Integer)
        s = (0, False, 0)
        lf (t, True, i) Tick         = (t+1, True, i+1)
        lf (t, b,    i) Tick         = (t+1, b,    i  )
        lf (t, _,    i) (KeyPress _) = (t,   True, i  )
        qf (3, _,    _) = True
        qf _            = False
        es = [Tick, KeyPress 'c', KeyPress 'c', Tick, Tick]
        g = Game nd s (const lf) nd qf
    it "does not confuse input and logic" $
      testGame g (createGRec (80, 24) es) `shouldBe` (3, True, 2)

  describe "testGame" $ do
    it "tests a game" $ do
        r <- readRecord "test/records/alone-record-test.gr"
        testGame aloneInARoom r `shouldBe` MyState (20, 66) Stop True
    it "picks up screen resize events" $ do
        r <- readRecord "test/records/balls-dims.gr"
        let g = fireworks (mkStdGen 1)
            t = testGame g r
        length (balls t) `shouldBe` 1
    it "picks up screen resize events" $ do
        r <- readRecord "test/records/balls-slow.gr"
        let g = fireworks (mkStdGen 1)
            t = testGame g r
        bslow t `shouldBe` True
    it "does not hang on empty/unclosed input" $
        let w = createGRec (80, 24) [Tick] in
        testGame aloneInARoom w `shouldBe` MyState (10, 10) Stop False
    modifyMaxSize (const 1000) $
      it "does not crash/hang on random input" $ Q.property $
        let genEvs = Q.listOf1 Q.arbitrary
        in Q.forAll genEvs $
             \es -> let w = createGRec (80, 24) es
                        a = testGame aloneInARoom w
                    in a == a
