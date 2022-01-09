module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (failures), runTestTT)

import Test.QR.Masking (maskingSuite)

main :: IO ()
main = do
  results <- runTestTT maskingSuite
  if failures results > 0 then exitFailure else exitSuccess
