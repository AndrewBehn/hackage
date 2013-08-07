{-# LANGUAGE Haskell2010, TemplateHaskell #-}

import Prelude
import qualified Prelude as P

import Control.Monad

import Numeric.Matrix

import Test.QuickCheck
import Test.QuickCheck.All

import System.Exit

import Data.Ratio
import Data.Int
import Data.Word


dim :: Num a => a
dim = 6

instance (MatrixElement e, Arbitrary e) => Arbitrary (Matrix e) where

    arbitrary = sequence (replicate dim (vector dim)) >>= return . fromList


basic m = m + m == scale m 2 && m * m * m == m ^ 3

prop_zero :: Word8 -> Bool
prop_zero = isZero . (zero :: Int -> Matrix Double) . fromIntegral

prop_sum_trace :: Word8 -> Bool
prop_sum_trace n = ((== nd) . P.sum . trace . (unit :: Int -> Matrix Double) . fromIntegral) n'
  where n' = n + 1
        nd = fromIntegral n'

prop_basic_integer :: Matrix Integer -> Bool
prop_basic_integer = basic

prop_basic_int :: Matrix Int -> Bool
prop_basic_int = basic

prop_basic_float :: Matrix Float -> Bool
prop_basic_float = basic

prop_basic_double :: Matrix Double -> Bool
prop_basic_double = basic

prop_basic_rational :: Matrix Rational -> Bool
prop_basic_rational = basic

prop_zero_det_double :: Matrix Double -> Bool
prop_zero_det_double m1 = let m1' = inv m1 in case m1' of (Just _) -> det m1 /= 0; _ -> det m1 == 0

prop_zero_det_rational :: Matrix Rational -> Bool
prop_zero_det_rational m1 = let m1' = inv m1 in case m1' of (Just _) -> det m1 /= 0; _ -> det m1 == 0

prop_plus_commutative :: Matrix Double -> Matrix Double -> Bool
prop_plus_commutative m1 m2 = m1 + m2 == m2 + m1

prop_inv :: Matrix Rational -> Bool
prop_inv m1 = let m1' = inv m1 in case m1' of (Just m1') -> m1' * m1 == unit dim; _ -> True

prop_rank :: Matrix Rational -> Bool
prop_rank m1 = let r = rank m1 in if r < dim then det m1 == 0 else r == dim

prop_trace_select :: Matrix Rational -> Bool
prop_trace_select m = trace m == select (uncurry (==)) m

prop_transpose_twice :: Matrix Rational -> Bool
prop_transpose_twice m = transpose (transpose m) == m

prop_inv_twice :: Matrix Rational -> Bool
prop_inv_twice m1 = let m1' = inv m1 in case m1' of (Just m1') -> inv m1' == Just m1; _ -> True

main = do
    success <- $(quickCheckAll)
    (if success then exitSuccess else exitFailure)


