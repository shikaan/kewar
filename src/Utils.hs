module Utils (readInt, toBin, toDec, leftPad, chunksOf, count, consecutiveChunksOf, leftUnpad, rightPad) where

import Data.Char (digitToInt, intToDigit)
import Data.List (foldl')
import Numeric (showIntAtBase)
import QR.Types (BitString)

readInt :: String -> Int
readInt = read

toBin :: Int -> BitString
toBin n = showIntAtBase 2 intToDigit n ""

toDec :: BitString -> Int
toDec = foldl' (\acc v -> 2 * acc + digitToInt v) 0

leftPad :: Int -> Char -> String -> String
leftPad size char str = replicate (size - length str) char ++ str

rightPad :: Int -> Char -> String -> String
rightPad size char str = str ++ replicate (size - length str) char

leftUnpad :: Char -> String -> String
leftUnpad char = dropWhile (==char)

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

consecutiveChunksOf :: Int -> [a] -> [[a]]
consecutiveChunksOf n l = [(take n . drop i) l | i <- [0 .. (length l - n)]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n l = step n l []
  where
    step size list acc = do
      if length list <= size
        then acc ++ [list]
        else do
          let (chunk, rest) = splitAt size list
          step size rest (acc ++ [chunk])