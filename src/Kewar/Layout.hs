module Kewar.Layout (placeBits, Grid, Module (..), cols) where

import Kewar.Layout.Data (dataBits)
import Kewar.Layout.FormatVersion (format, formatLocations, version, versionLocations)
import Kewar.Layout.FunctionalPatterns (functionalPatterns)
import Kewar.Layout.Interleaving (interleave)
import Kewar.Layout.Masking (optimalMask)
import Kewar.Layout.Types (Grid, Module (..), cols, insert, mkGrid)
import Kewar.Types (CorrectionLevel, Group, Version)
import Kewar.Layout.Constants (size)

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