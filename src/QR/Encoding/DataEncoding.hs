module QR.Encoding.DataEncoding where

import Data.Char (ord)
import QR.Constants (alphaNumericValue, characterCountIndicator, modeIndicator, totalBits)
import QR.Types (BitString, CorrectionLevel, Exception (..), Input, Mode (..), Version)
import Utils (chunksOf, leftPad, leftUnpad, readInt, toBin)

-- Convert Input to BitString
byteToBitString :: Input -> BitString
byteToBitString = concatMap (leftPad 8 '0' . (toBin . ord))

alphaNumericToBitString :: Input -> BitString
alphaNumericToBitString i = do
  let sums = map (\j -> if length j == 2 then (head j * 45) + last j else head j) (chunksOf 2 $ map alphaNumericValue i)
  let initial = concatMap (leftPad 11 '0' . toBin) (init sums)
  let finalPad = if odd $ length i then 6 else 11
  let final = leftPad finalPad '0' (toBin (last sums))
  initial ++ final

numericToBitString :: Input -> BitString
numericToBitString i = concatMap (step . leftUnpad '0') (chunksOf 3 i)
  where
    transform = toBin . readInt
    step chunk
      | length chunk == 1 = leftPad 4   '0' $ transform chunk
      | length chunk == 2 = leftPad 7   '0' $ transform chunk
      | otherwise         = leftPad 10  '0' $ transform chunk

toBitString :: Mode -> Input -> BitString
toBitString Numeric i = numericToBitString i
toBitString AlphaNumeric i = alphaNumericToBitString i
toBitString Byte i = byteToBitString i

-- TODO: handle errors
encode :: Input -> Mode -> Version -> CorrectionLevel -> Either Exception BitString
encode i m v cl = do
  let encoded = toBitString m i
  let requiredBits = totalBits v cl
  let t = terminator encoded requiredBits
  let mi = modeIndicator m
  let cci = characterCountIndicator i m v

  let encoded' = mi ++ cci ++ encoded ++ t
  -- to ensure multiple of 8
  let remainder = if s == 0 then 0 else 8 - s where s = length encoded' `mod` 8
  let t' = replicate remainder '0'
  let encoded'' = encoded' ++ t'
  let pbs = padBytes encoded'' requiredBits
  Right (encoded'' ++ pbs)

-- | Returns an approriate terminator string
terminator :: BitString -> Int -> BitString
terminator s requiredBits
  | delta >= 4 = "0000"
  | otherwise = replicate delta '0'
  where
    delta = requiredBits - length s

-- | Add alternating sequence of pad bytes to fill the string
padBytes :: BitString -> Int -> BitString
padBytes s requiredBits = do
  let numberOfPadBytes = (requiredBits - length s) `div` 8
  concatMap (\i -> if odd i then "11101100" else "00010001") [1 .. numberOfPadBytes]
