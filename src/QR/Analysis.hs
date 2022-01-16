module QR.Analysis (analyze) where

import Data.Char (isDigit, isLatin1)
import QR.Constants (allowedAlphaNumericValues)
import QR.Types (Exception (InvalidCharacterSet), Input, Mode (AlphaNumeric, Byte, Numeric))

analyze :: Input -> Either Exception Mode
analyze i
  | all isDigit i = Right Numeric
  | all (`elem` allowedAlphaNumericValues) i = Right AlphaNumeric
  | all isLatin1 i = Right Byte
  | otherwise = Left InvalidCharacterSet
