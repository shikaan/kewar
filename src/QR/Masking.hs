module QR.Masking where

import Data.Array (bounds, inRange, indices, (!))
import Data.List (foldl', groupBy)
import QR.ModulePlacement (Grid, Module (Black, White), Position, cols, rows)
import QR.Types (Exception (InvalidMask))

flip :: Module -> Module
flip Black = White
flip White = Black

mask :: Int -> (Position, Module) -> Either Exception (Position, Module)
mask typ ((r, c), m)
  | typ == 0 = apply $ even (r + c)
  | typ == 1 = apply $ even r
  | typ == 2 = apply $ c `mod` 3 == 0
  | typ == 3 = apply $ (r + c) `mod` 3 == 0
  | typ == 4 = apply $ even (floor (fromIntegral r / 2) + floor (fromIntegral c / 3))
  | typ == 5 = apply $ ((r * c `mod` 2) + (r * c `mod` 3)) == 0
  | typ == 6 = apply $ even ((r * c `mod` 2) + (r * c `mod` 3))
  | typ == 7 = apply $ even ((r + c `mod` 2) + (r * c `mod` 3))
  | otherwise = Left InvalidMask
  where
    apply rule = Right $ if rule then ((r, c), QR.Masking.flip m) else ((r, c), m)

sameModuleConsecutive :: [(Position, Module)] -> [[(Position, Module)]]
sameModuleConsecutive = groupBy (\(_, v) (_, v') -> v == v')

penalty1 :: Grid -> Int
penalty1 g = (penalty . rows) g + (penalty . cols) g
  where
    penalty gs = foldl' (\acc r -> acc + sum (map cost $ sameModuleConsecutive r)) 0 gs
    cost group
      | length group < 5 = 0
      | otherwise = length group - 2

penalty2 :: Grid -> Int
penalty2 g = foldl' (\acc b -> acc + cost b) 0 boxes
  where
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