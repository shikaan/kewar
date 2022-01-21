module QR.Encoding.ErrorEncoding where

import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (foldl', nub)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import QR.Constants (errorCorrectionCodeWordsPerBlock, fromExponent, toExponent)
import QR.Types (BitString, CorrectionLevel, Group, Version)
import Utils (leftPad, toBin, toDec)

-- | Returns a grouped list of Error CodeWords from a grouped Input
errorCodeWords :: [Group] -> CorrectionLevel -> Version -> [Group]
errorCodeWords groups cl version = do
  let eccw = errorCorrectionCodeWordsPerBlock version cl
  let errorCodeWordsPerBlock b = map (leftPad 8 '0' . toBin) (toList (divP (mkMessage b) (mkGenerator eccw)))

  map (map errorCodeWordsPerBlock) groups

-- Galois Field Polynomial whose entries are 0<=n<=255
type Polynomial = S.Seq Int

toBitString :: Polynomial -> String
toBitString = show . toList

fromBitString :: BitString -> Polynomial
fromBitString = mkPolynomial . map digitToInt

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

gfPower :: Int -> Int -> Int
gfPower a n = foldl' (\acc i -> QR.Encoding.ErrorEncoding.gfProduct acc a) 1 [1 .. n]

sumP :: Polynomial -> Polynomial -> Polynomial
sumP a b = S.zipWith gfSum (zeroFill a) (zeroFill b)
  where
    len = max (degree a) (degree b)
    zeroFill a = a S.>< S.replicate (len - degree a) 0

scaleP :: Polynomial -> Int -> Polynomial
scaleP p s = S.mapWithIndex (\_ i -> i `gfProduct` s) p

prodP :: Polynomial -> Polynomial -> Polynomial
prodP a b = do
  let indices = [(i, j) | i <- [0 .. S.length a -1], j <- [0 .. S.length b -1]]
  let degree = length $ nub (map (uncurry (+)) indices)
  let emp = mkPolynomial (replicate degree 0)
  foldl' (\acc (i, j) -> S.update (i + j) (acc ! (i + j) `gfSum` (a ! i `gfProduct` (b ! j))) acc) emp indices

divP :: Polynomial -> Polynomial -> Polynomial
divP dividend divisor = do
  let (dividend', divisor') = normalize dividend divisor
  let r = foldl' (step divisor') dividend' [1 .. (S.length dividend)]
  S.take (S.length r - degree dividend - 1) r
  where
    -- ensure dividend and divisor have same size
    normalize d d' = do
      ((S.><) d (S.replicate (degree d) 0), (S.><) d' (S.replicate (degree d') 0))

    step divisor dividend i = do
      let lead = dividend ! 0
      let divisor' = S.mapWithIndex (\_ a -> a `gfProduct` lead) divisor
      let dividend' = sumP dividend divisor'
      S.drop 1 dividend'

mkGenerator :: Int -> Polynomial
mkGenerator 1 = mkPolynomial [1, 1]
mkGenerator n = prodP (mkGenerator (n -1)) (mkPolynomial [1, fromExponent (n -1)])

mkMessage :: [BitString] -> Polynomial
mkMessage bs = mkPolynomial (map toDec bs)