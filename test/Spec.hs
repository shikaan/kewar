module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import qualified Test.QR.Encoding.Analysis
import qualified Test.QR.Encoding.Data
import qualified Test.QR.Layout.Masking

tests :: Test
tests =
  TestList [
    Test.QR.Encoding.Analysis.suite,
    Test.QR.Encoding.Data.suite,
    Test.QR.Layout.Masking.suite
  ]

main :: IO ()
main = do
  results <- runTestTT tests
  if failures results > 0 then exitFailure else exitSuccess
