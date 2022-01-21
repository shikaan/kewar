module QR.Encoding.Grouping (groups) where

import QR.Types (BitString, Version, CorrectionLevel, Group)
import QR.Constants (groupsCodeWords)
import Utils (chunksOf)
import Data.Foldable (foldl')

-- | Returns a list of groups of blocks for a given bitstring
groups :: BitString -> Version -> CorrectionLevel -> [Group]
groups input version correctionLevel = do
  let gcw = groupsCodeWords version correctionLevel
  let dataCodewords = chunksOf 8 input :: [BitString]
  fst $ foldl' (\(acc, cw) (gs, size) -> (acc ++ [take gs (chunksOf size cw)], drop (gs * size) cw)) ([], dataCodewords) gcw