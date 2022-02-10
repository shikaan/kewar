module Kewar.Encoding.Analysis (mode, version) where

import Data.Char (isDigit, isLatin1)
import Kewar.Constants (allowedAlphaNumericValues, capacities)
import Kewar.Types (Exception (InvalidCharacterSet), Mode (AlphaNumeric, Byte, Numeric), Version, CorrectionLevel)
import Data.List (find)
import Data.Maybe (fromJust)

mode :: String -> Either Exception Mode
mode i
  | all isDigit i = Right Numeric
  | all (`elem` allowedAlphaNumericValues) i = Right AlphaNumeric
  | all isLatin1 i = Right Byte
  | otherwise = Left InvalidCharacterSet

version :: String -> Mode -> CorrectionLevel -> Version
version i m cl = fst $ fromJust $ find (\(_, limit) -> limit > s) cs
  where
    cs = capacities cl m
    s = length i
  