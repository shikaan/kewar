module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import qualified Test.Kewar.Encoding.Analysis
import qualified Test.Kewar.Encoding.Data
import qualified Test.Kewar.Layout.Masking

tests :: Test
tests =
  TestList [
    Test.Kewar.Encoding.Analysis.suite,
    Test.Kewar.Encoding.Data.suite,
    Test.Kewar.Layout.Masking.suite
  ]

main :: IO ()
main = do
  results <- runTestTT tests
  if failures results > 0 then exitFailure else exitSuccess
