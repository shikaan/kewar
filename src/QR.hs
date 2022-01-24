-- |
-- Copyright: (c) 2022 Manuel Spagnolo
-- SPDX-License-Identifier: MIT
-- Maintainer: Manuel Spagnolo <spagnolo.manu@gmail.com>
--
-- See README for more info
module QR
  ( generate,
    CorrectionLevel (..),
    Grid,
    Module (..),
    cols,
  )
where

import QR.Encoding (encodeData, encodeError, mode, version)
import QR.Layout (Grid, Module (..), cols, placeBits)
import QR.Types (CorrectionLevel (..), Exception, Input, Mode)

generate :: Input -> CorrectionLevel -> Either Exception Grid
generate i cl = case maybeMode of
  Left e -> Left e
  Right m -> Right $ generateWithMode i cl m
  where
    maybeMode = mode i

generateWithMode :: Input -> CorrectionLevel -> Mode -> Grid
generateWithMode i cl m = do
  let v = version i m cl
  let encoded = encodeData i m v cl
  let ecw = encodeError encoded cl v
  placeBits v cl encoded ecw