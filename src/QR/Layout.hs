module QR.Layout (placeBits, Grid, Module (..), cols) where

import QR.Layout.Data (dataBits)
import QR.Layout.FormatVersion (format, formatLocations, version, versionLocations)
import QR.Layout.FunctionalPatterns (functionalPatterns)
import QR.Layout.Interleaving (interleave)
import QR.Layout.Masking (optimalMask)
import QR.Layout.Types (Grid, Module (..), cols, insert, mkGrid)
import QR.Types (CorrectionLevel, Group, Version)
import QR.Layout.Constants (size)

placeBits :: Version -> CorrectionLevel -> [Group] -> [Group] -> Grid
placeBits v cl dataCodeWords errorCodeWords = do
  let s = size v
  let g = mkGrid ((0, 0), (s -1, s -1))

  let fps = functionalPatterns v
  let functionalPatternsLocations = map fst fps
  let forbiddenLocations = concat (formatLocations v) ++ concat (versionLocations v) ++ functionalPatternsLocations

  let interleaved = interleave v dataCodeWords errorCodeWords
  let g' = insert g (fps ++ dataBits s interleaved forbiddenLocations)

  let (masked, pattern) = optimalMask forbiddenLocations g'
  let formatModules = format v cl pattern
  let versionModules = version v

  insert masked (formatModules ++ versionModules)