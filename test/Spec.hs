module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (failures), Test (..), assertEqual, runTestTT)

test1 :: Test
test1 = TestCase (assertEqual "label" "1" "2")

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
  results <- runTestTT tests
  if failures results > 0 then exitFailure else exitSuccess
