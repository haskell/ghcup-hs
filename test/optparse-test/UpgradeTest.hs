{-# LANGUAGE TupleSections #-}

module UpgradeTest where

import Test.Tasty
import GHCup.OptParse
import Utils


upgradeTests :: TestTree
upgradeTests = buildTestTree upgradeParseWith ("upgrade", upgradeCheckList)

type FullUpgradeOpts =
  ( UpgradeOpts
  , Bool -- ^Force update
  , Bool -- ^Fails after upgrading if the upgraded ghcup binary is shadowed by something else in PATH (useful for CI)
  )

mkDefaultOptions :: UpgradeOpts -> FullUpgradeOpts
mkDefaultOptions = (, False, False)

upgradeCheckList :: [(String, FullUpgradeOpts)]
upgradeCheckList =
  [ ("upgrade", mkDefaultOptions UpgradeGHCupDir)
  , ("upgrade -f", (UpgradeGHCupDir, True, False))
  , ("upgrade --force", (UpgradeGHCupDir, True, False))
  , ("upgrade --fail-if-shadowed", (UpgradeGHCupDir, False, True))
  , ("upgrade -i", mkDefaultOptions UpgradeInplace)
  , ("upgrade --inplace", mkDefaultOptions UpgradeInplace)
  , ("upgrade -t ~", mkDefaultOptions $ UpgradeAt "~")
  , ("upgrade --target ~", mkDefaultOptions $ UpgradeAt "~")
  , ("upgrade -t ~ -f", (UpgradeAt "~", True, False))
  ]

upgradeParseWith :: [String] -> IO FullUpgradeOpts
upgradeParseWith args = do
  Upgrade a b c <- parseWith args
  pure (a, b, c)
