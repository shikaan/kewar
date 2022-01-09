module QR.Types
  ( Input,
    Mode (..),
    CorrectionLevel (..),
    Exception (..),
    Version,
    BitString,
    Codeword,
    Block,
    Group,
  )
where

type Input = String

data Mode = Numeric | AlphaNumeric | Byte | Kanji deriving (Eq, Show, Enum)

data CorrectionLevel = L | M | Q | H deriving (Eq, Enum)

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

data Exception = InvalidCharacterSet | InvalidVersionOrMode | NotImplemented | InvalidMask deriving (Eq, Show, Enum)