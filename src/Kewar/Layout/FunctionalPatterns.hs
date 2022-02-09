module Kewar.Layout.FunctionalPatterns (functionalPatterns) where

import Kewar.Constants (alignmentPatternLocations)
import Kewar.Layout.Constants (size)
import Kewar.Layout.Types
  ( Module (..),
    Position,
    moveTo,
    overlapsWith,
    sumP,
  )
import Kewar.Types (Version)

functionalPatterns :: Version -> [(Position, Module)]
functionalPatterns v = finders v ++ alignments v ++ timings v ++ darkModule v ++ separators v

finders :: Version -> [(Position, Module)]
finders v = do
  let whitePositions = [(1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (1, 2), (1, 3), (1, 4), (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (5, 2), (5, 3), (5, 4)]
  let whites = zip whitePositions (repeat White)
  let blacks = zip [(i, j) | i <- [0 .. 6], j <- [0 .. 6], (i, j) `notElem` whitePositions] (repeat Black)
  let finder = blacks ++ whites

  finder ++ moveTo (s -7, 0) finder ++ moveTo (0, s -7) finder
  where
    s = size v

alignments :: Version -> [(Position, Module)]
alignments 1 = []
alignments v = do
  -- Draw blocks
  let whitePositions = [(1, 1), (2, 1), (3, 1), (1, 2), (1, 3), (2, 3), (3, 3), (3, 2)]
  let whites = zip whitePositions (repeat White)
  let blacks = zip [(i, j) | i <- [0 .. 4], j <- [0 .. 4], (i, j) `notElem` whitePositions] (repeat Black)
  let alignment = blacks ++ whites

  -- Filter out location which would yield a block overlapping with finders
  let findersRanges = [((0, 0), (7, 7)), ((s -8, 0), (s -1, 7)), ((0, s -8), (7, s -1))]
  let locations' = filter (\l -> not (any (overlapsWithRange l) findersRanges)) $ alignmentPatternLocations v

  concatMap (`moveTo` alignment) locations'
  where
    corners position = [position, sumP position (4, 4), sumP position (0, 4), sumP position (4, 0)]
    overlapsWithRange position range = any (`overlapsWith` range) (corners position)
    s = size v

timings :: Version -> [(Position, Module)]
timings v = do
  let b = s - 16
  let vertical = [((i, 0), if odd i then Black else White) | i <- [1 .. b]]
  let horizontal = [((0, i), if odd i then Black else White) | i <- [1 .. b]]

  moveTo (6, 7) horizontal ++ moveTo (7, 6) vertical
  where
    s = size v

darkModule :: Version -> [(Position, Module)]
darkModule v = [(((4 * v) + 9, 8), Black)]

separators :: Version -> [(Position, Module)]
separators v = do
  let topLeft = [((i, 7), White) | i <- [0 .. 7]] ++ [((7, i), White) | i <- [0 .. 6]]
  let topRight = [((i, s - 8), White) | i <- [0 .. 7]] ++ [((7, i), White) | i <- [s -7 .. s -1]]
  let bottomLeft = [((s - 8, i), White) | i <- [0 .. 7]] ++ [((i, 7), White) | i <- [s -7 .. s -1]]

  topLeft ++ topRight ++ bottomLeft
  where
    s = size v