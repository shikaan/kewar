{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Kewar.Layout.FormatVersion (format, version, formatLocations, versionLocations) where

import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Kewar.Constants (formatBitString, versionBitString)
import Kewar.Layout.Constants (size)
import Kewar.Layout.Types (Module, Position, fromChar)
import Kewar.Types (CorrectionLevel, Version)

version :: Version -> [(Position, Module)]
version v = zip bottomLeft modules ++ zip topRight modules
  where
    modules = map (fromJust . fromChar) $ fromMaybe "" $ versionBitString v
    bottomLeft : topRight : _ = versionLocations v

format :: Int -> CorrectionLevel -> Int -> [(Position, Module)]
format v cl pattern = zip tl modules ++ zip (concat rest) modules
  where
    modules = map (fromJust . fromChar) $ formatBitString cl pattern
    tl : rest = formatLocations v

versionLocations :: Version -> [[Position]]
versionLocations v
  | v < 7 = [[], []]
  | otherwise = [l, map swap l]
  where
    s = size v
    l = [(j, i) | j <- [0 .. 5], i <- [s -11 .. s -9]]

formatLocations :: Int -> [[Position]]
formatLocations v = do
  let topRight = [(8, i) | i <- reverse [s -7 .. s -1]]
  let bottomLeft = [(i, 8) | i <- [s -8 .. s -1]]
  let topLeft = [(i, 8) | i <- [0 .. 5] ++ [7]] ++ [(8, i) | i <- reverse ([0 .. 5] ++ [7, 8])]
  [topLeft, topRight, bottomLeft]
  where
    s = size v