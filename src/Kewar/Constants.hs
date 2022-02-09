module Kewar.Constants
  ( capacities,
    modeIndicator,
    characterCountIndicator,
    totalBits,
    toExponent,
    fromExponent,
    groupsCodeWords,
    errorCorrectionCodeWordsPerBlock,
    remainderBits,
    alignmentPatternLocations,
    alphaNumericValue,
    allowedAlphaNumericValues,
    formatBitString,
    versionBitString,
  )
where

import Data.IntMap (IntMap, fromList, (!))
import Data.List (find, foldl', isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Paths_kewar (getDataFileName)
import Kewar.Types (BitString, CorrectionLevel, Input, Mode (AlphaNumeric, Byte, Numeric), Version)
import System.IO.Unsafe (unsafePerformIO)
import Utils (leftPad, readInt, toBin)

-- | Reads unsafely CSV data files from data-files. Not really proud.
unsafeReadCSVFile :: FilePath -> [[String]]
unsafeReadCSVFile fileName = map (splitOn ",") (lines fileAsString)
  where
    fileAsString = unsafePerformIO $ readFile $ unsafePerformIO $ getDataFileName fileName

-- | Returns bit string indicator for the Mode
modeIndicator :: Mode -> BitString
modeIndicator Numeric = "0001"
modeIndicator AlphaNumeric = "0010"
modeIndicator Byte = "0100"

-- | Returns character capacity per Version
capacities :: CorrectionLevel -> Mode -> [(Version, Int)]
capacities correctionLevel mode = do
  let perCorrectionLevel = filter (\c -> c !! 1 == show correctionLevel) rawCapacities
  foldl' (\acc c -> acc ++ [(readInt $ head c, readInt $ c !! (modeIndex mode + 2))]) [] perCorrectionLevel

characterCountIndicatorSize :: Version -> Mode -> Int
characterCountIndicatorSize v m
  | v `elem` [1 .. 9] = [10, 9, 8, 8] !! i
  | v `elem` [10 .. 26] = [12, 11, 16, 10] !! i
  | otherwise = [14, 13, 16, 12] !! i
  where
    i = modeIndex m

-- | Returns character count indicator, a binary, left-padded string
-- representing the length of the input
characterCountIndicator :: Input -> Mode -> Version -> BitString
characterCountIndicator i m v = leftPad (characterCountIndicatorSize v m) '0' binaryLength
  where
    binaryLength = toBin (length i)

-- | Returns total number of required bits
totalBits :: Version -> CorrectionLevel -> Int
totalBits v cl = readInt (r !! 2) * 8
  where
    r = errorCodewordAndBlock v cl

-- | Returns a list of tuples such that (#Groups, #CodeWords per group) per group (1-based)
-- e.g. (groupsCodeWords version cl) !! 0 -> returns groups codeword information for group 1
groupsCodeWords :: Version -> CorrectionLevel -> [(Int, Int)]
groupsCodeWords v cl = [(readInt (r !! 4), readInt (r !! 5)), (readInt (r !! 6), readInt (r !! 7))]
  where
    r = errorCodewordAndBlock v cl

errorCorrectionCodeWordsPerBlock :: Version -> CorrectionLevel -> Int
errorCorrectionCodeWordsPerBlock v cl = readInt $ errorCodewordAndBlock v cl !! 3

remainderBits :: Version -> Int
remainderBits v = readInt (record !! 1)
  where
    record = fromJust $ find (\i -> head i == show v) rawRemainderBits

alignmentPatternLocations :: Version -> [(Int, Int)]
alignmentPatternLocations v = [(x -2, y -2) | x <- baseCoordinates, y <- baseCoordinates] 
  where
    baseCoordinates = (Map.!) rawAlignmentPatternLocations v

alphaNumericValue :: Char -> Int
alphaNumericValue = (Map.!) rawAlphaNumericValues

allowedAlphaNumericValues :: [Char]
allowedAlphaNumericValues = Map.keys rawAlphaNumericValues

formatBitString :: CorrectionLevel -> Int -> BitString
formatBitString errorCorrection maskPattern = record !! 2
  where
    record = fromJust $ find ([show errorCorrection, show maskPattern] `isPrefixOf`) rawFormatString

versionBitString :: Version -> Maybe BitString
versionBitString v
  | v < 7 = Nothing
  | otherwise = do
    case find (\i -> show v == head i) rawVersionString of
      Just record -> Just $ record !! 1
      Nothing -> Nothing

-- | DUMP

-- | Return the index of mode column in following tables
modeIndex :: Mode -> Int
modeIndex Numeric = 0
modeIndex AlphaNumeric = 1
modeIndex Byte = 2

-- Version-Correction, Numeric, AlphaNumeric, Byte
rawCapacities :: [[String]]
rawCapacities = unsafeReadCSVFile "capacities.csv"

-- EC = Error Correction, B= Block, CW= CodeWords
-- Version, EC, Total #CW, ECCW per B, #B in G1, #CW per G1's B, #B in G2, CW per G2's B
rawErrorCodewordsAndBlock :: [[String]]
rawErrorCodewordsAndBlock = unsafeReadCSVFile "errors.csv"

rawAlignmentPatternLocations :: Map.Map Int [Int]
rawAlignmentPatternLocations = Map.fromList $ map split $ unsafeReadCSVFile "alignment.csv"
  where
    split l = (readInt x, map readInt xs) where x : xs = l

rawRemainderBits :: [[String]]
rawRemainderBits = unsafeReadCSVFile "remainder.csv"

-- | We control both Version and CorrectionLevel, no need to handle Maybe
errorCodewordAndBlock :: Version -> CorrectionLevel -> [String]
errorCodewordAndBlock v cl = fromJust $ find (\r -> take 2 r == [show v, show cl]) rawErrorCodewordsAndBlock

rawLogTable :: [(Int, Int)]
rawLogTable = map split $ unsafeReadCSVFile "logarithms.csv"
  where
    split l = (readInt $ head l, readInt $ last l)

rawAlphaNumericValues :: Map.Map Char Int
rawAlphaNumericValues = Map.fromList s
  where
    s = map (\i -> ((head . head) i, (readInt . last) i)) (unsafeReadCSVFile "alphanumeric.csv")

exponents :: IntMap Int
exponents = fromList rawLogTable

numbers :: IntMap Int
numbers = fromList (map swap rawLogTable)

toExponent :: Int -> Int
toExponent 0 = 0
toExponent a = exponents ! a

fromExponent :: Int -> Int
fromExponent a = numbers ! a

rawFormatString :: [[String]]
rawFormatString = unsafeReadCSVFile "format.csv"

rawVersionString :: [[String]]
rawVersionString = unsafeReadCSVFile "version.csv"
