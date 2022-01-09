module QR.ModulePlacement (draw, showG, Grid, Position, Module(..)) where

import Data.Array (Array, array, assocs, bounds, elems, inRange, indices, listArray, (//))
import Data.Bifunctor (bimap, first)
import Data.Foldable (foldl')
import Data.List (intercalate)
import QR.Constants (alignmentPatternLocations)
import QR.Types (BitString, Version)
import Utils (chunksOf)

draw :: Version -> BitString -> (Grid, [Position])
draw v bs = do
  let fs = finders v
  let ss = separators v
  let ts = timings v
  let as = alignments v
  let dm = darkModule v

  let g = mkGrid (sizeG v)
  let functionalPatterns = fs ++ as ++ ts ++ dm ++ ss
  let forbiddenLocations = formatArea v ++ versionArea v ++ map fst functionalPatterns

  (g // (fs ++ as ++ ts ++ dm ++ ss ++ dataBits v bs forbiddenLocations), forbiddenLocations)

data Module = Black | White deriving (Eq)

----

instance Show Module where
  show Black = "██"
  show White = "  "

type Position = (Int, Int)

type Size = (Position, Position)

type Grid = Array Position Module

showG :: Grid -> String
showG g = do
  let c = chunksOf (snd (snd (bounds g)) + 1) (map show (elems g))
  intercalate "\n" (map (intercalate "") c)

singleton :: Array Position Module
singleton = listArray ((0, 0), (0, 0)) [White]

mkGrid :: Size -> Grid
mkGrid b = listArray b (repeat White)

sumP :: Position -> Position -> Position
sumP a = bimap (fst a +) (snd a +)

maxP :: Position -> Position -> Position
maxP a = bimap (max (fst a)) (max (snd a))

sizeG :: Version -> Size
sizeG v = ((0, 0), (s, s)) where s = size v - 1

size :: Version -> Int
size v = ((v -1) * 4) + 21

translateTo :: Position -> [(Position, Module)] -> [(Position, Module)]
translateTo p = map (first (sumP p))

finders :: Version -> [(Position, Module)]
finders v = do
  let whitePositions = [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (5, 2), (5, 3), (5, 4)]
  let whites = zip whitePositions (repeat White)
  let blacks = zip [(i, j) | i <- [0 .. 6], j <- [0 .. 6], (i, j) `notElem` whitePositions] (repeat Black)
  let finder = blacks ++ whites

  finder ++ translateTo (size v -7, 0) finder ++ translateTo (0, size v -7) finder

alignments :: Version -> [(Position, Module)]
alignments 1 = []
alignments v = do
  -- Draw blocks
  let whitePositions = [(1, 1), (2, 1), (3, 1), (1, 2), (1, 3), (2, 3), (3, 3), (3, 2)]
  let whites = zip whitePositions (repeat White)
  let blacks = zip [(i, j) | i <- [0 .. 4], j <- [0 .. 4], (i, j) `notElem` whitePositions] (repeat Black)
  let alignment = blacks ++ whites

  -- Position the blocks
  let locationBaseCoordinates = alignmentPatternLocations v
  let locations = [(x -2, y -2) | x <- locationBaseCoordinates, y <- locationBaseCoordinates]

  -- Filter out location which would yield a block overlapping with finders
  let s = size v
  let forbiddenRanges = [((0, 0), (7, 7)), ((s -8, 0), (s -1, 7)), ((0, s -8), (7, s -1))]
  let locations' = filter (\l -> all (\r -> not (inRange r l) && not (inRange r $ sumP l (4, 4)) && not (inRange r $ sumP l (0, 4)) && not (inRange r $ sumP l (4, 0))) forbiddenRanges) locations

  concatMap (`translateTo` alignment) locations'

timings :: Version -> [(Position, Module)]
timings v = do
  let s = size v
  let b = s - 16
  let vertical = [((i, 0), if odd i then Black else White) | i <- [1 .. b]]
  let horizontal = [((0, i), if odd i then Black else White) | i <- [1 .. b]]

  translateTo (6, 7) horizontal ++ translateTo (7, 6) vertical

darkModule :: Version -> [(Position, Module)]
darkModule v = [(((4 * v) + 9, 8), Black)]

separators v = do
  let topLeft = [((i, 7), White) | i <- [0 .. 7]] ++ [((7, i), White) | i <- [0 .. 6]]
  let topRight = [((i, s - 8), White) | i <- [0 .. 7]] ++ [((7, i), White) | i <- [s -7 .. s -1]]
  let bottomLeft = [((s - 8, i), White) | i <- [0 .. 7]] ++ [((i, 7), White) | i <- [s -7 .. s -1]]

  topLeft ++ topRight ++ bottomLeft
  where
    s = size v

formatArea v = do
  let topRight = [(8, i) | i <- [s -8 .. s -1]]
  let bottomLeft = [(i, 8) | i <- [s -7 .. s -1]]
  let topLeft = [(i, 8) | i <- [0 .. 5] ++ [7]] ++ [(8, i) | i <- [0 .. 5] ++ [7, 8]]
  topRight ++ topLeft ++ bottomLeft
  where
    s = size v

versionArea v
  | v < 7 = []
  | otherwise = do
    let topRight = [(i, j) | i <- [s -11 .. s -9], j <- [0 .. 5]]
    let bottomLeft = [(j, i) | i <- [s -11 .. s -9], j <- [0 .. 5]]
    topRight ++ bottomLeft
  where
    s = size v

---
-- starting from teh bottom right point
column (a,b) = [(j, i) | j <- [b -1, b -2 .. 0], i <- [a-1, a-2]]

dataBits :: Version -> BitString -> [Position] -> [(Position, Module)]
dataBits version bitString forbiddenLocations = do
  let inColumns = concatMap column (zip [s, s -2 .. 0] (repeat s))
  let allowedLocations = filter (`notElem` forbiddenLocations) inColumns
  zipWith (\l b -> (l, if b == '0' then White else Black)) allowedLocations bitString
  where
    s = size version