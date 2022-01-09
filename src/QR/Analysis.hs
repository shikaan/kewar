module QR.Analysis (analyze) where

import QR.Types (Input, Mode(Byte, AlphaNumeric))

analyze :: Input -> Mode
analyze i = AlphaNumeric
