module QR.Encoding
  ( encode,
    version,
    characterCountIndicator,
    groups,
  )
where

import Data.Char (isLatin1, ord)
import Data.Either (fromRight)
import Data.Foldable (foldl')
import QR.Constants (allowedAlphaNumericValues, alphaNumericValue, capacities, characterCountIndicatorSize, groupsCodeWords, modeIndicator, totalBits)
import QR.Types (BitString, CorrectionLevel, Exception (InvalidCharacterSet, NotImplemented), Group, Input, Mode (AlphaNumeric, Byte, Kanji, Numeric), Version)
import Utils (chunksOf, leftPad, toBin)

-- Returns minimum Version
version :: Input -> Mode -> CorrectionLevel -> Int
version input mode correctionLevel = do
  let cs = capacities correctionLevel mode
  fst (head (filter (\(v, limit) -> limit > length input) cs))

characterCountIndicator :: Input -> Mode -> Version -> Either Exception BitString
characterCountIndicator i m v =
  case eitherCCIS of
    Right ccis -> Right (leftPad ccis '0' bSize)
    Left error -> Left error
  where
    eitherCCIS = characterCountIndicatorSize v m
    bSize = toBin (length i)

-- Convert Input to BitString
bytetoBitString :: Input -> Either Exception BitString
bytetoBitString input
  | not (all isLatin1 input) = Left InvalidCharacterSet
  | otherwise = Right (concatMap (leftPad 8 '0' . (toBin . ord)) input)

alphaNumericToBitString :: Input -> Either Exception BitString
alphaNumericToBitString i
  | not (all (`elem` allowedAlphaNumericValues) i) = Left InvalidCharacterSet
  | otherwise = do
    let sums = map (\i -> if length i == 2 then (head i * 45) + last i else head i) (chunksOf 2 $ map alphaNumericValue i)
    let initial = concatMap (leftPad 11 '0' . toBin) (init sums)
    let finalPad = if odd $ length i then 6 else 11
    let final = leftPad finalPad '0' (toBin (last sums))
    Right (initial ++ final)

toBitString :: Mode -> Input -> Either Exception BitString
toBitString Numeric i = Left NotImplemented
toBitString AlphaNumeric i = alphaNumericToBitString i
toBitString Byte i = bytetoBitString i
toBitString Kanji i = Left NotImplemented

-- TODO: handle errors
encode :: Input -> Mode -> Version -> CorrectionLevel -> Either Exception BitString
encode i m v cl = do
  let encoded = fromRight "" $ toBitString m i
  let requiredBits = fromRight 0 (totalBits v cl)
  let t = terminator encoded requiredBits
  let mi = modeIndicator m
  let Right cci = characterCountIndicator i m v

  let encoded' = mi ++ cci ++ encoded ++ t
  -- to ensure multiple of 8
  let remainder = if s == 0 then 0 else 8 - s where s = length encoded' `mod` 8
  let t' = replicate remainder '0'
  let encoded'' = encoded' ++ t'
  let pbs = padBytes encoded'' requiredBits
  Right (encoded'' ++ pbs)

-- Returns a list of groups of blocks for a given bitstring
-- Group = List of Blocks, Block = List of Bitstrings
groups :: BitString -> Version -> CorrectionLevel -> Either Exception [Group]
groups input version correctionLevel = do
  let gcw = fromRight [] (groupsCodeWords version correctionLevel)
  let dataCodewords = chunksOf 8 input :: [BitString]
  let (groups, _) = foldl' (\(acc, cw) (groups, size) -> (acc ++ [take groups (chunksOf size cw)], drop (groups * size) cw)) ([], dataCodewords) gcw
  Right groups

-- Returns an approriate terminator string
terminator :: BitString -> Int -> BitString
terminator s requiredBits
  | delta >= 4 = "0000"
  | otherwise = replicate delta '0'
  where
    delta = requiredBits - length s

-- Add alternating sequence of pad bytes to fill the string
padBytes :: BitString -> Int -> BitString
padBytes s requiredBits = do
  let numberOfPadBytes = (requiredBits - length s) `div` 8
  concatMap (\i -> if odd i then "11101100" else "00010001") [1 .. numberOfPadBytes]
