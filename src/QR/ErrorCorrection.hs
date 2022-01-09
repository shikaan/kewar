module QR.ErrorCorrection (errorCodeWords) where

import Data.Bits (xor)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.List (foldl', nub)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as S
import QR.Constants (errorCorrectionCodeWordsPerBlock, fromExponent, groupsCodeWords, modeIndicator, toExponent)
import QR.Types (BitString, Codeword, CorrectionLevel, Exception, Group, Mode, Version)
import Utils (chunksOf, leftPad, toBin, toDec)

errorCodeWords :: [Group] -> CorrectionLevel -> Version -> Either Exception [Group]
errorCodeWords groups cl version = do
  let eccw = fromRight 0 (errorCorrectionCodeWordsPerBlock version cl)
  let errorCodeWordsPerBlock b = map (leftPad 8 '0' . toBin) (toList (divP (message b) (generator $ eccw -1)))

  Right (map (map errorCodeWordsPerBlock) groups)

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

gfPower :: Int -> Int -> Int
gfPower a n = foldl' (\acc i -> QR.ErrorCorrection.gfProduct acc a) 1 [1 .. n]

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
  S.take (S.length r - degree dividend) r
  where
    -- ensure dividend and divisor have same size
    normalize d d' = do
      ((S.><) d (S.replicate (degree d) 0), (S.><) d' (S.replicate (degree d') 0))

    step divisor dividend i = do
      let lead = dividend ! 0
      let divisor' = S.mapWithIndex (\_ a -> a `gfProduct` lead) divisor
      let dividend' = sumP dividend divisor'
      S.drop 1 dividend'

generator :: Int -> Polynomial
generator 1 = mkPolynomial [1, 1]
generator n = prodP (generator (n -1)) (mkPolynomial [1, fromExponent (n -1)])

message :: [BitString] -> Polynomial
message bs = mkPolynomial (map toDec bs)