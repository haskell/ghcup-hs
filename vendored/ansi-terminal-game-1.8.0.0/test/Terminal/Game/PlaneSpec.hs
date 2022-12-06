module Terminal.Game.PlaneSpec where

import Test.Hspec
import Terminal.Game.Plane
import Terminal.Game.Draw

import qualified Control.Exception as E


spec :: Spec
spec = do

  let testPlane =         blankPlane 2 2 &
                  (1,1) % box 2 2 '.'    &
                  (1,2) % cell ' '

  describe "listPlane" $ do
    it "creates a plane from string" $
      listPlane (2,2) (map creaCell ". ..") `shouldBe` testPlane
    it "ignores extra characters" $
      listPlane (2,2) (map creaCell ". ..abc") `shouldBe` testPlane

  describe "pastePlane" $ do
    it "pastes a simple plane onto another" $
      pastePlane (cell 'a') (cell 'b') (1,1) `shouldBe` cell 'a'

  describe "stringPlane" $ do
    it "creates plane from spec" $
      stringPlane ".\n.." `shouldBe` testPlane

  describe "stringPlaneTrans" $ do
    it "allows transparency" $
      stringPlaneTrans '.' ".\n.." `shouldBe` makeTransparent '.' testPlane

  describe "updatePlane" $ do
    let ma = listPlane (2,1) (map creaCell "ab")
        mb = listPlane (2,1) (map creaCell "xb")
    it "updates a Plane" $
      updatePlane ma [((1,1), creaCell 'x')] `shouldBe` mb

  describe "subPlane" $ do
    let pa = word "prova" === word "fol"
    it "cuts out a plane" $
      planePaper (subPlane pa (1, 1) (2, 1)) `shouldBe` "p\nf\n"
    it "does not crash on OOB" $
      planeSize (subPlane pa (1, 1) (10, 10)) `shouldBe` (5, 2)
    it "errs on emptycell" $
      E.evaluate (subPlane pa (2, 3) (1, 1)) `shouldThrow`
        errorCall "subPlane: top-left point (2,3) > bottom-right point (1,1)."
    it "but not on a single cell" $
      subPlane pa (2, 3) (2, 3) `shouldBe` cell 'l'

  describe "hcat/vcat" $ do
    let pa = blankPlane 2 1
        pb = blankPlane 3 4
    it "concats planes horizontally with hcat" $
      planeSize (hcat [pa, pb]) `shouldBe` (5, 4)
    it "concats planes horizontally with vcat" $
      planeSize (vcat [pa, pb]) `shouldBe` (3, 5)
