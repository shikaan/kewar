module Kewar.CLI.Grid (showG) where

import Data.List (intercalate)
import Kewar (Grid, Module(..), cols)

join :: [[Char]] -> [Char]
join = intercalate ""

showM :: Module -> [Char]
showM Black = "██"
showM White = "  "

showG :: Grid -> String
showG g = unlines $ map (join . map (showM . snd)) columns
  where
    columns = cols g