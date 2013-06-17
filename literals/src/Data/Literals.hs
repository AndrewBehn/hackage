{-# LANGUAGE Haskell2010 #-}

module Data.Literals where

import Data.Int
import Data.Word


int x = x :: Int

integer x = x :: Integer


int8 x = x :: Int8

int16 x = x :: Int16

int32 x = x :: Int32

int64 x = x :: Int64


word8 x = x :: Word8

word16 x = x :: Word16

word32 x = x :: Word32

word64 x = x :: Word64


float x = x :: Float

double x = x :: Double



