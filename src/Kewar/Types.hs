module Kewar.Types
  ( Mode (..),
    CorrectionLevel (..),
    Exception (..),
    Version,
    BitString,
    Codeword,
    Block,
    Group,
  )
where

data Mode = Numeric | AlphaNumeric | Byte deriving (Eq, Show)

-- | Correction Level allows reading QR codes in case they get damaged or unreadable.
data CorrectionLevel
  = -- | allows up to 7% data recovery
    L
  | -- | allows up to 15% data recovery
    M
  | -- | allows up to 25% data recovery
    Q
  | -- | allows up to 30% data recovery
    H
  deriving (Eq)

instance Show CorrectionLevel where
  show L = "L"
  show M = "M"
  show Q = "Q"
  show H = "H"

type Version = Int

type BitString = String -- TODO: how to enforce chars?

type Codeword = BitString -- of length 8. TODO: how to enforce length?

type Block = [BitString]

type Group = [Block]

data Exception = InvalidCharacterSet | InvalidMask deriving (Eq, Enum)

instance Show Exception where
  show InvalidCharacterSet = "Input character set is not supported. Please provide a valid ISO 8859-1 string."
  show InvalidMask = "Provided mask is not valid. Please provide a value between 0-7."