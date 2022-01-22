module QR.Encoding.Error (encodeError) where

import Data.Bits (xor)
import Data.Foldable (toList)
import Data.List (foldl', nub)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import QR.Constants (errorCorrectionCodeWordsPerBlock, fromExponent, toExponent)
import QR.Types (BitString, CorrectionLevel, Group, Version)
import Utils (leftPad, toBin, toDec)

-- | Returns a grouped list of Error CodeWords from a grouped Input
encodeError :: [Group] -> CorrectionLevel -> Version -> [Group]
encodeError groups cl version = map (map $ encodeErrorBlock requiredCodeWords) groups
  where
    requiredCodeWords = errorCorrectionCodeWordsPerBlock version cl

encodeErrorBlock :: Int -> [BitString] -> [BitString]
encodeErrorBlock requiredCodeWords block = map (leftPad 8 '0' . toBin) $ toList (divP message generator)
  where
    message = mkMessage block
    generator = mkGenerator requiredCodeWords

-- Galois Field Polynomial whose entries are 0<=n<=255
type Polynomial = S.Seq Int

degree :: Polynomial -> Int
degree p = S.length p - 1

(!) :: Polynomial -> Int -> Int
(!) a i = fromMaybe 0 (S.lookup i a)

mkPolynomial :: [Int] -> S.Seq Int
mkPolynomial = S.fromList

gfSum :: Int -> Int -> Int
gfSum a b = a `xor` b

gfProduct :: Int -> Int -> Int
gfProduct a b
  | a == 0 || b == 0 = 0
  | otherwise = fromExponent ((toExponent a + toExponent b) `mod` 255)

-- | Ensure two Polynomials have the same degree
normalize :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
normalize pa pb = ((S.><) pa (S.replicate (degree pa) 0), (S.><) pb (S.replicate (degree pb) 0))

sumP :: Polynomial -> Polynomial -> Polynomial
sumP a b = S.zipWith gfSum a' b'
  where
    (a', b') = normalize a b

prodP :: Polynomial -> Polynomial -> Polynomial
prodP a b = do
  let indices = [(i, j) | i <- [0 .. S.length a -1], j <- [0 .. S.length b -1]]
  let deg = length $ nub (map (uncurry (+)) indices)
  let emp = mkPolynomial (replicate deg 0)
  foldl' (\acc (i, j) -> S.update (i + j) (acc ! (i + j) `gfSum` (a ! i `gfProduct` (b ! j))) acc) emp indices

divP :: Polynomial -> Polynomial -> Polynomial
divP dividend divisor = do
  let (dividend', divisor') = normalize dividend divisor
  let r = foldl' (divisionStep divisor') dividend' [1 .. (S.length dividend)]
  S.take (S.length r - degree dividend - 1) r
  where
    divisionStep pa pb _ = S.drop 1 $ sumP pb pa'
      where
        pa' = S.mapWithIndex (\_ a -> a `gfProduct` (pb ! 0)) pa

mkGenerator :: Int -> Polynomial
mkGenerator 1 = mkPolynomial [1, 1]
mkGenerator n = prodP (mkGenerator (n -1)) (mkPolynomial [1, fromExponent (n -1)])

mkMessage :: [BitString] -> Polynomial
mkMessage bs = mkPolynomial (map toDec bs)