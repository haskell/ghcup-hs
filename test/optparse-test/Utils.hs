{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Utils where

import GHCup.OptParse as GHCup
import Options.Applicative
import Data.Bifunctor
import Data.Versions
import Data.List.NonEmpty (NonEmpty)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.IO.Class
import qualified Data.Text as T

parseWith :: [String] -> IO Command
parseWith args =
  optCommand <$> handleParseResult
    (execParserPure defaultPrefs (info GHCup.opts fullDesc) args)

padLeft :: Int -> String -> String
padLeft desiredLength s = padding ++ s
  where padding = replicate (desiredLength - length s) ' '

mapSecond :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSecond = map . second

mkVersion :: NonEmpty VChunk -> Version
mkVersion chunks = Version Nothing chunks [] Nothing

mkVersion' :: T.Text -> Version
mkVersion' txt =
  let Right ver = version txt
  in ver

buildTestTree
  :: (Eq a, Show a)
  => ([String] -> IO a) -- ^ The parse function
  -> (String, [(String, a)]) -- ^ The check list @(test group, [(cli command, expected value)])@
  -> TestTree
buildTestTree parse (title, checkList) =
  testGroup title
    $ zipWith (uncurry . check) [1 :: Int ..] checkList
  where
    check idx args expected = testCase (padLeft 2 (show idx) ++ "." ++ args) $ do
      res <- parse (words args)
      liftIO $ res @?= expected
