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

import QR.Encoding (encode, errorCodeWords, groups, mode, version)
import QR.Layout (Grid, Module (..), cols, draw, interleave, mainFV, optimalMask, rows)
import QR.Types (CorrectionLevel (..), Exception, Input)

--- Return QR Code
generate :: Input -> CorrectionLevel -> Either Exception Grid
generate i cl = do
  let Right m = mode i
  let v = version i m cl
  let Right encoded = encode i m v cl

  let gs = groups encoded v cl
  let ecw = errorCodeWords gs cl v
  let bs = interleave v gs ecw

  let (g, ps) = draw v bs
  let (masked, mv) = optimalMask ps g
  Right (mainFV masked v cl mv)
