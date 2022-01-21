module QR.Encoding.Analysis where

import Data.Char (isDigit, isLatin1)
import QR.Constants (allowedAlphaNumericValues, capacities)
import QR.Types (Exception (InvalidCharacterSet), Input, Mode (AlphaNumeric, Byte, Numeric), Version, CorrectionLevel)

mode :: Input -> Either Exception Mode
mode i
  | all isDigit i = Right Numeric
  | all (`elem` allowedAlphaNumericValues) i = Right AlphaNumeric
  | all isLatin1 i = Right Byte
  | otherwise = Left InvalidCharacterSet

version :: Input -> Mode -> CorrectionLevel -> Version
version input mode correctionLevel = do
  let cs = capacities correctionLevel mode
  fst (head (filter (\(v, limit) -> limit > length input) cs))