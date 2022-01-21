module QR.Layout.FormatVersion where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import QR.Constants (format, version)
import QR.Encoding.ErrorEncoding (fromBitString, sumP, toBitString)
import QR.Layout.ModulePlacement (fromChar, insert, size)
import QR.Types (BitString, CorrectionLevel (H, L, M, Q))
import Utils (leftPad, leftUnpad, rightPad, toBin)

formatLocations v = do
  [ [(0, 8), (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (7, 8), (8, 8), (8, 7), (8, 5), (8, 4), (8, 3), (8, 2), (8, 1), (8, 0)],
    [(8, s), (8, s -1), (8, s -2), (8, s -3), (8, s -4), (8, s -5), (8, s -6), (s -7, 8), (s -6, 8), (s -5, 8), (s -4, 8), (s -3, 8), (s -2, 8), (s -1, 8), (s, 8)]
    ]
  where
    s = (size v) - 1

versionLocations v = do
  let x = [(0, s -11), (0, s -10), (0, s -9), (1, s -11), (1, s -10), (1, s -9), (2, s -11), (2, s -10), (2, s -9), (3, s -11), (3, s -10), (3, s -9), (4, s -11), (4, s -10), (4, s -9), (5, s -11), (4, s -10), (4, s -9)]
  [x, map swap x]
  where
    s = size v

mainFV g v cl mp = do
  let vS = version v
  let vl = versionLocations v
  let (vs1', vs2') = case vS of Just vs -> (zip (head vl) (map (fromJust . fromChar) vs), zip (last vl) (map (fromJust . fromChar) vs)); Nothing -> ([], [])

  let fS = format cl mp
  let fl = formatLocations v
  let fl1' = zip (head fl) (map (fromJust . fromChar) fS)
  let fl2' = zip (last fl) (map (fromJust . fromChar) fS)

  insert g (vs1' ++ vs2' ++ fl1' ++ fl2')
