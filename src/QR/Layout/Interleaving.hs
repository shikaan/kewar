module QR.Layout.Interleaving (interleave) where

import Data.List (transpose)
import QR.Constants (remainderBits)
import QR.Types (BitString, Group, Version)

interleave :: Version -> [Group] -> [Group] -> BitString
interleave v dataGroups errorGroups
  | length dataGroups == 1 = concat ((concat . concat) dataGroups ++ (concat . concat) errorGroups)
  | otherwise = do
    let dataBlocks = concat dataGroups
    let errorBlocks = concat errorGroups
    let interleaved = concat (concat (transpose dataBlocks) ++ concat (transpose errorBlocks))
    let remainder = replicate (remainderBits v) '0'

    interleaved ++ remainder