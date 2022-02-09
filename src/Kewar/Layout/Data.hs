module Kewar.Layout.Data (dataBits) where

import Kewar.Layout.Types (Module (..), Position)
import Kewar.Types (BitString)

dataBits :: Int -> BitString -> [Position] -> [(Position, Module)]
dataBits s bitString forbiddenLocations = do
  zipWith (\l b -> (l, if b == '0' then White else Black)) allowedLocations bitString
  where
    n = s -1
    line (x, r) =
      [ c | y <- if r then [n, n -1 .. 0] else [0 .. n], c <- [(x, y), (x -1, y)]
      ]
    columns =
      zip
        ([n, n -2 .. 8] ++ [5, 3, 1])
        (cycle [True, False])
    cs = columns >>= line
    allowedLocations = filter (`notElem` forbiddenLocations) cs