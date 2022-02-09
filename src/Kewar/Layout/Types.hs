module Kewar.Layout.Types (Module (..), Position, Grid, fromChar, flipM, sumP, maxP, mkGrid, insertAt, insert, transpose, rows, cols, dimension, moveTo, overlapsWith) where

import Data.Array
import Data.Bifunctor (bimap, first)
import Data.List (groupBy)
import Data.Tuple (swap)

data Module = Black | White deriving (Eq)

fromChar :: Char -> Maybe Module
fromChar '0' = Just White
fromChar '1' = Just Black
fromChar _ = Nothing

flipM :: Module -> Module
flipM Black = White
flipM White = Black

type Position = (Int, Int)

sumP :: Position -> Position -> Position
sumP a = bimap (fst a +) (snd a +)

maxP :: Position -> Position -> Position
maxP a = bimap (max (fst a)) (max (snd a))

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

rows :: Grid -> [[(Position, Module)]]
rows g = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs g)

cols :: Grid -> [[(Position, Module)]]
cols g = groupBy (\((a, _), _) ((c, _), _) -> a == c) (assocs $ transpose g)

dimension :: Grid -> Int
dimension g = (r + 1) * (c + 1) where (r, c) = snd $ bounds g

moveTo :: Position -> [(Position, Module)] -> [(Position, Module)]
moveTo p = map $ first (sumP p)

overlapsWith :: Position -> (Position, Position) -> Bool
overlapsWith p r = inRange r p