module QR.Masking where

import Data.Array (assocs, bounds, inRange, index, indices, ixmap, (!))
import Data.Either (Either (Right))
import Data.List (foldl', groupBy)
import Data.Tuple (swap)
import QR.ModulePlacement (Grid, Module (Black, White), Position)
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

-- TODO: move me
transpose :: Grid -> Grid
transpose g = ixmap (bounds g) swap g

-- end moveme

penalty1 :: Grid -> Int
penalty1 g = do
  let rows = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs g)
  let cols = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs $ transpose g)
  let rPenalty = foldl' (\acc r -> acc + sum (map cost $ sameModuleConsecutive r)) 0 rows
  let cPenalty = foldl' (\acc c -> acc + sum (map cost $ sameModuleConsecutive c)) 0 cols
  rPenalty + cPenalty
  where
    cost group
      | length group < 5 = 0
      | otherwise = 3 + (length group - 5)

penalty2 :: Grid -> Int
penalty2 g = foldl' (\acc b -> acc + cost b) 0 boxes
  where
    bnds = bounds g
    
    neighbors :: Position -> [Position]
    neighbors (a, b) = [(a, b), (a + 1, b), (a, b + 1), (a + 1, b + 1)]
    
    step :: [[(Position, Module)]] -> (Position, Module) -> [[(Position, Module)]]
    step l ((a, b), m)
      | inRange bnds (a + 1, b + 1) = l ++ [map (\p -> (p, m)) (neighbors (a, b))]
      | otherwise = l
    
    boxes = foldl' step [] (assocs g)

    cost :: [(Position, Module)] -> Int
    cost group
      | length (sameModuleConsecutive group) == 1 = 3
      | otherwise = 0