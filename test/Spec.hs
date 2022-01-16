module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import qualified Test.QR.Analysis
import qualified Test.QR.Masking

tests :: Test
tests =
  TestList
    [ Test.QR.Masking.suite,
      Test.QR.Analysis.suite
    ]

main :: IO ()
main = do
  results <- runTestTT tests
  if failures results > 0 then exitFailure else exitSuccess
