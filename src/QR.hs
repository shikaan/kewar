-- |
-- Copyright: (c) 2022 Manuel Spagnolo
-- SPDX-License-Identifier: MIT
-- Maintainer: Manuel Spagnolo <spagnolo.manu@gmail.com>
--
-- See README for more info
module QR
  ( generate,
    CorrectionLevel (..),
    Module (..),
    Grid,
    rows,
    cols,
  )
where

import QR.Analysis (analyze)
import QR.Encoding (encode, groups, version)
import QR.ErrorCorrection (errorCodeWords)
import QR.FormatVersion (mainFV)
import QR.Interleaving (interleave)
import QR.Masking (optimalMask)
import QR.ModulePlacement (Grid, Module (..), cols, draw, rows)
import QR.Types (CorrectionLevel (..), Exception, Input)

--- Return QR Code
generate :: Input -> CorrectionLevel -> Either Exception Grid
generate i cl = do
  let m = analyze i
  let v = version i m cl
  let Right encoded = encode i m v cl

  let Right gs = groups encoded v cl
  let Right ecw = errorCodeWords gs cl v
  let bs = interleave v gs ecw

  let (g, ps) = draw v bs
  let (masked, mv) = optimalMask ps g
  Right (mainFV masked v cl mv)
