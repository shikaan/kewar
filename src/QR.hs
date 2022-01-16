{- |
Copyright: (c) 2022 Manuel Spagnolo
SPDX-License-Identifier: MIT
Maintainer: Manuel Spagnolo <spagnolo.manu@gmail.com>

See README for more info
-}

module QR
  ( generate,
    CorrectionLevel (..),
    showG,
  )
where

import QR.Analysis (analyze)
import QR.Encoding (encode, groups, version)
import QR.ErrorCorrection (errorCodeWords)
import QR.Interleaving (interleave)
import QR.ModulePlacement (Grid, draw, showG)
import QR.Masking (optimalMask)
import QR.FormatVersion (mainFV)
import QR.Types (CorrectionLevel (..), Input, Exception)

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
