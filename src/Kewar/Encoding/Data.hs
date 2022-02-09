module Kewar.Encoding.Data (encodeData, toBitString) where

import Data.Char (ord)
import Kewar.Constants (alphaNumericValue, characterCountIndicator, modeIndicator, totalBits, groupsCodeWords)
import Kewar.Types (BitString, CorrectionLevel, Input, Mode (..), Version, Group)
import Utils (chunksOf, leftPad, leftUnpad, readInt, toBin)
import Data.Foldable (foldl')

-- | Encodes an input string to a BitString with length as per Kewar specification
encodeData :: Input -> Mode -> Version -> CorrectionLevel -> [Group]
encodeData i m v cl = groups (byteString ++ padBytes byteString requiredBits) v cl
  where
    requiredBits = totalBits v cl
    encoded = basicEncodeData i m v
    byteString = toByteString $ encoded ++ terminator encoded requiredBits

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
      | length chunk == 1 = leftPad 4 '0' $ transform chunk
      | length chunk == 2 = leftPad 7 '0' $ transform chunk
      | otherwise = leftPad 10 '0' $ transform chunk

toBitString :: Mode -> Input -> BitString
toBitString Numeric i = numericToBitString i
toBitString AlphaNumeric i = alphaNumericToBitString i
toBitString Byte i = byteToBitString i

-- | Converts input to BitString and chains it with mode indicator and character count indicator
basicEncodeData :: Input -> Mode -> Version -> BitString
basicEncodeData i m v = modeIndicator m ++ characterCountIndicator i m v ++ toBitString m i

-- | Takes a BitString and ensures its length is multiple of 8 by adding 0s
toByteString :: BitString -> BitString
toByteString s
  | rest == 0 = s
  | otherwise = s ++ replicate (8 - rest) '0'
  where
    rest = length s `mod` 8

-- | Return as many '0' as needed to fill required length.
-- Terminator string cannot be longer than 4 chars
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

-- | Returns a list of groups of blocks for a given bitstring
groups :: BitString -> Version -> CorrectionLevel -> [Group]
groups input version correctionLevel = do
  let gcw = groupsCodeWords version correctionLevel
  let dataCodewords = chunksOf 8 input :: [BitString]
  fst $ foldl' (\(acc, cw) (gs, size) -> (acc ++ [take gs (chunksOf size cw)], drop (gs * size) cw)) ([], dataCodewords) gcw