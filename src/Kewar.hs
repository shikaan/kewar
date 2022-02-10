-- |
-- Copyright: (c) 2022 Manuel Spagnolo
-- SPDX-License-Identifier: MIT
-- Maintainer: Manuel Spagnolo <spagnolo.manu@gmail.com>
--
-- Generate a QR code from a given input string.
-- The output of the entry point `generate` is an array representing the drawable representation of the QR code,
-- meaning a data structure that can be mapped - for instance - to terminal output or images.
--
-- The module includes a CLI utility which prints the QR code directly in the terminal as an example.
--
-- You can install it via
--
-- @
-- cabal install exe:kewar
-- @
--
-- And use it like
--
-- @
-- kewar "Hello World"
-- ██████████████  ██  ██  ████████    ██████████████
-- ██          ██    ██████  ████████  ██          ██
-- ██  ██████  ██        ██████  ██    ██  ██████  ██
-- ██  ██████  ██      ██      ████    ██  ██████  ██
-- ██  ██████  ██  ████  ██      ████  ██  ██████  ██
-- ██          ██  ██    ██            ██          ██
-- ██████████████  ██  ██  ██  ██  ██  ██████████████
--                         ██  ██  ██                
--   ██████████████                ██    ████      ██
--       ██████  ██  ██████  ██████      ██    ██    
--     ██████  ██  ██  ████  ██████████    ████  ████
--       ██      ██████████  ██  ██    ██        ████
-- ██  ██    ██████      ██    ████  ████████████████
-- ██  ████████      ████████    ████    ██    ██    
-- ██          ██  ██      ██  ██    ██████████  ████
-- ██  ██    ██  ██  ████  ██  ██  ██  ██████      ██
-- ██      ██  ██  ████  ██        ██████████████    
--                                 ██      ██  ██  ██
-- ██████████████  ██████  ████  ████  ██  ██  ██████
-- ██          ██  ██  ██    ████████      ████  ██  
-- ██  ██████  ██  ██  ████      ████████████████    
-- ██  ██████  ██  ████        ██      ██  ████    ██
-- ██  ██████  ██  ██    ██    ████    ████  ██    ██
-- ██          ██  ██  ██████    ██  ██  ████      ██
-- ██████████████    ████  ████    ██    ██      ████
-- @

module Kewar
  ( generate,
    CorrectionLevel (..),
    Grid,
    cols,
    rows,
    Module (..),
    Position,
  )
where

import Kewar.Encoding (encodeData, encodeError, mode, version)
import Kewar.Layout (Grid, Module (..), Position, cols, placeBits, rows)
import Kewar.Types (CorrectionLevel (..), Exception, Mode)

-- | Entry point of the library. Attempts generating a QR Code Grid from a given input string.
generate :: String -> CorrectionLevel -> Either Exception Grid
generate i cl = case maybeMode of
  Left e -> Left e
  Right m -> Right $ generateWithMode i cl m
  where
    maybeMode = mode i

generateWithMode :: String -> CorrectionLevel -> Mode -> Grid
generateWithMode i cl m = do
  let v = version i m cl
  let encoded = encodeData i m v cl
  let ecw = encodeError encoded cl v
  placeBits v cl encoded ecw