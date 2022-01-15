module QR.Constants
  ( capacities,
    modeIndicator,
    characterCountIndicatorSize,
    totalBits,
    toExponent,
    fromExponent,
    groupsCodeWords,
    errorCorrectionCodeWordsPerBlock,
    remainderBits,
    alignmentPatternLocations,
    alphaNumericValue,
    allowedAlphaNumericValues,
    format,
    version,
  )
where

import Data.IntMap (IntMap, fromList, (!))
import Data.List (find, foldl', isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Paths_qr (getDataFileName)
import QR.Types (BitString, CorrectionLevel, Exception (InvalidVersionOrMode), Mode (AlphaNumeric, Byte, Kanji, Numeric), Version)
import System.IO.Unsafe (unsafePerformIO)
import Utils (readInt)

{- Reads unsafely CSV data files from data-files. Not really proud. -}
unsafeReadCSVFile :: FilePath -> [[String]]
unsafeReadCSVFile fileName = do
  let s = unsafePerformIO $ readFile $ unsafePerformIO $ getDataFileName fileName
  map (splitOn ",") (lines s)

-- Returns bit string indicator for the Mode
modeIndicator :: Mode -> BitString
modeIndicator Numeric = "0001"
modeIndicator AlphaNumeric = "0010"
modeIndicator Byte = "0100"
modeIndicator Kanji = "1000"

-- Returns character capacity per Version
capacities :: CorrectionLevel -> Mode -> [(Version, Int)]
capacities correctionLevel mode = do
  let perCorrectionLevel = filter (\c -> c !! 1 == show correctionLevel) rawCapacities
  foldl' (\acc c -> acc ++ [(readInt $ head c, readInt $ c !! (modeIndex mode + 2))]) [] perCorrectionLevel

-- Returns size of character count indicator
characterCountIndicatorSize :: Version -> Mode -> Either Exception Int
characterCountIndicatorSize v m
  | v >= 1 && v <= 9 = Right ([10, 9, 8, 8] !! i)
  | v >= 10 && v <= 26 = Right ([12, 11, 16, 10] !! i)
  | v >= 27 && v <= 40 = Right ([14, 13, 16, 12] !! i)
  | otherwise = Left InvalidVersionOrMode
  where
    i = modeIndex m

-- Returns total number of required bits
totalBits :: Version -> CorrectionLevel -> Either Exception Int
totalBits v cl = do
  case errorCodewordAndBlock v cl of
    Just r -> Right (readInt (r !! 2) * 8)
    Nothing -> Left InvalidVersionOrMode

-- Returns a list of tuples such that (#Groups, #CodeWords per group) per group (1-based)
-- e.g. (groupsCodeWords version cl) !! 0 -> returns groups codeword information for group 1
groupsCodeWords :: Version -> CorrectionLevel -> Either Exception [(Int, Int)]
groupsCodeWords v cl = do
  case errorCodewordAndBlock v cl of
    Just r -> Right [(readInt (r !! 4), readInt (r !! 5)), (readInt (r !! 6), readInt (r !! 7))]
    Nothing -> Left InvalidVersionOrMode

errorCorrectionCodeWordsPerBlock :: Version -> CorrectionLevel -> Either Exception Int
errorCorrectionCodeWordsPerBlock v cl = do
  case errorCodewordAndBlock v cl of
    Just r -> Right (readInt (r !! 3))
    Nothing -> Left InvalidVersionOrMode

remainderBits :: Version -> Int
remainderBits v = readInt (record !! 1)
  where
    record = fromJust $ find (\i -> head i == show v) rawRemainderBits

alignmentPatternLocations :: Version -> [Int]
alignmentPatternLocations = (Map.!) rawAlignmentPatternLocations

alphaNumericValue :: Char -> Int
alphaNumericValue = (Map.!) rawAlphaNumericValues

allowedAlphaNumericValues :: [Char]
allowedAlphaNumericValues = Map.keys rawAlphaNumericValues

format :: CorrectionLevel -> Int -> BitString
format errorCorrection maskPattern = record !! 2
  where
    record = fromJust $ find ([show errorCorrection, show maskPattern] `isPrefixOf`) rawFormatString

version :: Version -> Maybe BitString
version v
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
modeIndex Kanji = 3

-- Version-Correction, Numeric, AlphaNumeric, Byte, Kanji
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

errorCodewordAndBlock :: (Show a1, Show a2) => a1 -> a2 -> Maybe [String]
errorCodewordAndBlock v cl = find (\r -> take 2 r == [show v, show cl]) rawErrorCodewordsAndBlock

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
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
