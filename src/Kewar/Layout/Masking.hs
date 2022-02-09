module Kewar.Layout.Masking (mask, penalty1, penalty2, penalty3, penalty4, optimalMask, maskGrid) where

import Data.Array (bounds, elems, inRange, indices, (!))
import Data.Either (fromRight)
import Data.List (elemIndex, foldl', foldl1', groupBy)
import Data.Maybe (fromJust)
import Kewar.Layout.Types (Grid, Module (..), Position, cols, dimension, flipM, insert, rows)
import Kewar.Types (Exception (InvalidMask))
import Utils (consecutiveChunksOf, count)

mask :: Int -> (Position, Module) -> Either Exception (Position, Module)
mask typ ((c, r), m)
  | typ == 0 = flipIf $ even (r + c)
  | typ == 1 = flipIf $ even r
  | typ == 2 = flipIf $ c `mod` 3 == 0
  | typ == 3 = flipIf $ (r + c) `mod` 3 == 0
  | typ == 4 = flipIf $ even $ floor (fromIntegral r / 2) + floor (fromIntegral c / 3)
  | typ == 5 = flipIf $ ((r * c `mod` 2) + (r * c `mod` 3)) == 0
  | typ == 6 = flipIf $ even ((r * c `mod` 2) + (r * c `mod` 3))
  | typ == 7 = flipIf $ even ((r + c `mod` 2) + (r * c `mod` 3))
  | otherwise = Left InvalidMask
  where
    flipIf :: Bool -> Either undefined (Position, Module)
    flipIf rule = Right $ if rule then ((c, r), flipM m) else ((c, r), m)

sameModuleConsecutive :: [(Position, Module)] -> [[(Position, Module)]]
sameModuleConsecutive = groupBy (\(_, v) (_, v') -> v == v')

penalty1 :: Grid -> Int
penalty1 g = (calc . rows) g + (calc . cols) g
  where
    calc gs = foldl' (\acc r -> acc + sum (map cost $ sameModuleConsecutive r)) 0 gs
    cost group
      | length group < 5 = 0
      | otherwise = length group - 2

penalty2 :: Grid -> Int
penalty2 g = foldl' (\acc b -> acc + cost b) 0 boxes
  where
    bnds :: (Position, Position)
    bnds = bounds g

    neighbors :: Position -> [Position]
    neighbors (a, b) = [(a + i, b + j) | i <- [0, 1], j <- [0, 1]]

    step :: [[(Position, Module)]] -> Position -> [[(Position, Module)]]
    step l (a, b)
      | inRange bnds (a + 1, b + 1) = l ++ [map (\p -> (p, g ! p)) $ neighbors (a, b)]
      | otherwise = l

    boxes :: [[(Position, Module)]]
    boxes = foldl' step [] (indices g)

    cost :: [(Position, Module)] -> Int
    cost group
      | length (sameModuleConsecutive group) == 1 = 3
      | otherwise = 0

penalty3 :: Grid -> Int
penalty3 g = (calc . map modules . rows) g + (calc . map modules . cols) g
  where
    -- Patterns to be matched
    pattern1 = [Black, White, Black, Black, Black, White, Black, White, White, White, White]
    pattern2 = [White, White, White, White, Black, White, Black, Black, Black, White, Black]

    matches :: [Module] -> Int
    matches l = length $ filter (\i -> i == pattern1 || i == pattern2) $ consecutiveChunksOf 11 l

    modules :: [(a, b)] -> [b]
    modules l = map snd l

    calc gs = 40 * foldl' (\s g' -> s + matches g') 0 gs

penalty4 :: Grid -> Int
penalty4 g = m * 10
  where
    total = dimension g
    blacks = count (== Black) (elems g)
    ratio = (100 * blacks) `div` total
    q = ratio `mod` 5
    m = minimum $ map (\x -> abs (x - 50) `div` 5) [ratio - q, ratio - q + 5]

penalty :: Grid -> Int
penalty g = sum [penalty1 g, penalty2 g, penalty3 g, penalty4 g]

maskGrid :: Grid -> [Position] -> Int -> Grid
maskGrid g ps n = insert g (map (\p -> fromRight (p, g ! p) $ mask n (p, g ! p)) ps)

optimalMask :: [Position] -> Grid -> (Grid, Int)
optimalMask forbiddenLocations grid = (masked !! minMask, minMask)
  where
    locations = filter (`notElem` forbiddenLocations) (indices grid)
    masked = map (maskGrid grid locations) [0 .. 7]
    penalties = map penalty masked
    minMask = fromJust $ elemIndex (foldl1' min penalties) penalties