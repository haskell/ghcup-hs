{-# LANGUAGE CPP #-}

module GHCup.Prelude.File.Posix.TraversalsSpec where


#if !defined(IS_WINDOWS)
import           GHCup.Prelude.File.Posix.Traversals

import           Control.Monad.IO.Class (liftIO)
import           Data.List
import           System.Posix.Directory
import           Unsafe.Coerce
#endif

import           Test.Hspec



spec :: Spec
spec = do
#if defined(IS_WINDOWS)
  pure ()
#else
  -- https://github.com/haskell/ghcup-hs/issues/415
  describe "GHCup.Prelude.File.Posix.Traversals" $ do
    it "readDirEnt" $ do
      dirstream <- liftIO $ openDirStream "test/data"
      (dt1, fp1) <- readDirEnt dirstream
      (dt2, fp2) <- readDirEnt dirstream
      (dt3, fp3) <- readDirEnt dirstream
      (dt4, fp4) <- readDirEnt dirstream
      let xs = sortOn snd [ (dt1, fp1), (dt2, fp2)
                          , (dt3, fp3), (dt4, fp4)
                          ]
      xs `shouldBe` [(unsafeCoerce (4 :: Int),".")
                    ,(unsafeCoerce (4 :: Int),"..")
                    ,(unsafeCoerce (4 :: Int),"dir")
                    ,(unsafeCoerce (8 :: Int),"file")
                    ]
#endif
