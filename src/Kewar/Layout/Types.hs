module Kewar.Layout.Types (Module (..), Position, Grid, fromChar, flipM, sumP, maxP, mkGrid, insertAt, insert, transpose, rows, cols, dimension, moveTo, overlapsWith) where

import Data.Array
import Data.Bifunctor (bimap, first)
import Data.List (groupBy)
import Data.Tuple (swap)

-- | A module represent a single unit composing a QR code. We're not using the word `pixel` as they usually span more than a pixel.
data Module
  = -- | Dark module
    Black
  | -- | Light module
    White
  deriving (Eq)

fromChar :: Char -> Maybe Module
fromChar '0' = Just White
fromChar '1' = Just Black
fromChar _ = Nothing

flipM :: Module -> Module
flipM Black = White
flipM White = Black

-- | Position of a module in a given grid. It's in the form (row index, column index).
type Position = (Int, Int)

sumP :: Position -> Position -> Position
sumP a = bimap (fst a +) (snd a +)

maxP :: Position -> Position -> Position
maxP a = bimap (max (fst a)) (max (snd a))

-- | Array holding the drawable representation of the QR code. It includes encoded data, functional patterns, format, and version bits.
type Grid = Array Position Module

type Size = (Position, Position)

mkGrid :: Size -> Grid
mkGrid b = listArray b (repeat White)

insertAt :: Grid -> Position -> Module -> Grid
insertAt g p m = g // [(p, m)]

insert :: Grid -> [(Position, Module)] -> Grid
insert g as = g // as

transpose :: Grid -> Grid
transpose g = ixmap (bounds g) swap g

-- | Returns a list of rows for a given grid. Useful for drawing.
rows :: Grid -> [[(Position, Module)]]
rows g = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs g)

-- | Returns a list of columns for a given grid. Useful for drawing.
cols :: Grid -> [[(Position, Module)]]
cols g = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs $ transpose g)

dimension :: Grid -> Int
dimension g = (r + 1) * (c + 1) where (r, c) = snd $ bounds g

moveTo :: Position -> [(Position, Module)] -> [(Position, Module)]
moveTo p = map $ first (sumP p)

overlapsWith :: Position -> (Position, Position) -> Bool
overlapsWith p r = inRange r p