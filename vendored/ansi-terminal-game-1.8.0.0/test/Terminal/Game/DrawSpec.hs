module Terminal.Game.DrawSpec where

import Test.Hspec
import Terminal.Game.Plane
import Terminal.Game.Draw
import Terminal.Game -- language hyphenators


spec :: Spec
spec = do

  describe "mergePlanes" $ do
    it "piles multiple planes together" $
      mergePlanes (stringPlane "aa")
                  [((1,2), cell 'b')] `shouldBe` stringPlane "ab"
    it "works in the middle too" $
      mergePlanes (stringPlane "aaa\naaa\naaa")
                  [((2,2), cell 'b')] `shouldBe`
                    stringPlane "aaa\naba\naaa"

  describe "textBox/textBoxLiquid" $ do
    let s  = "las rana in Spa"
        w  = 6
        ps = textBox w 2 s
        pl = textBoxLiquid w s
    it "textBox follows specific size" $
      planeSize ps `shouldBe` (6, 2)
    it "textBoxLiquid fits the whole string" $
      planeSize pl `shouldBe` (6, 3)
    it "textBox should make a transparent plane" $
      let p1 = textBox 6 1 "a c e "
          p2 = textBox 6 1 " b d f"
          pc = p1 & (1, 1) % p2
      in planePaper pc `shouldBe` "abcdef\n"

  describe "textBoxHypen" $ do
    let tbh = textBoxHyphen spanish 8 2 "Con pianito"
    it "hyphens long words" $
      planePaper tbh `shouldSatisfy` elem '-'

  describe "***" $ do
    let a  = stringPlane ".\n.\n.\n"
        b  = stringPlane "*"
        c  = stringPlane ".\n*\n.\n"
    it "blits b in the centre of a" $
      a *** b `shouldBe` c

  -- combinators

  let sp = stringPlane "ab"
      bp = blankPlane 4 3

  describe "%^>" $ do
    it "blits in the top right corner" $
      planePaper (bp & (1,1) %^> sp) `shouldBe` "  ab\n    \n    \n"

  describe "%_<" $ do
    it "blits in the bottom left corner" $
      planePaper (bp & (2,1) %.< sp) `shouldBe` "    \nab  \n    \n"

  describe "%_<" $ do
    it "blits in the bottom left corner" $
      planePaper (bp & (2,3) %.> sp) `shouldBe` "    \nab  \n    \n"

  describe "%" $ do
    it "mixes with alternative combinators" $
      planePaper (bp & (1,2) % sp & (2,3) %.> sp) `shouldBe`
        " ab \nab  \n    \n"
