{-# LANGUAGE Haskell2010,
             ScopedTypeVariables,
             FlexibleInstances,
             TypeSynonymInstances,
             OverlappingInstances #-}
{-# OPTIONS -Wall -O2 #-}


module Text.DeadSimpleJSON.Convert (
    Value (..),
    Convert (..)
) where


import Prelude hiding (True, False)
import qualified Prelude

import Data.Int
import Data.Ratio
import Text.DeadSimpleJSON.Types

import qualified Data.Vector as V
-- import qualified Data.Map as M


default ()


class Convert a where
    convert :: Value -> a

    convert' :: Value -> Maybe a
    convert' n@(Number _ _) = return $ convert n
    convert' _ = fail ""

    toJSON :: a -> Value
    toJSON = undefined


instance Convert a => Convert (Maybe a) where
    convert  = convert'
    convert' = maybe Nothing Just . convert'

    toJSON (Just a) = toJSON a
    toJSON Nothing = Null


instance Convert Value where
    convert = id
    convert' = return

    toJSON = id


instance Convert Bool where
    convert True  = Prelude.True
    convert (String "") = Prelude.False
    convert (String _) = Prelude.True
    convert (Number 0 _) = Prelude.False
    convert (Number _ _) = Prelude.True
    convert _ = Prelude.False

    convert' True = return Prelude.True
    convert' False = return Prelude.False
    convert' _ = fail ""

    toJSON (Prelude.True) = True
    toJSON (Prelude.False) = False


instance Convert String where
    convert (String s) = s
    convert (Number n e)
        | e >= 10   = show e
        | otherwise = show n ++ "e" ++ show e
    convert (Array arr) = tail $ tail $ V.foldr (\v l -> ", " ++ convert v ++ l) "" arr
    convert (Object _) = "{object}"
    convert True  = "true"
    convert False = "false"
    convert Null  = "null"

    convert' (String s) = return s
    convert' _ = fail ""

    toJSON = String


instance Convert a => Convert [a] where
    convert (Array v) = map convert $ V.toList v
    convert _ = []

    convert' arr@(Array _) = return $ convert arr
    convert' _ = fail ""

    toJSON = Array . V.fromList . map toJSON


instance Convert Double where
    convert (Number n e)
        | e >= 0    = fromInteger (n * 10 ^ e) / 1.0
        | otherwise = fromInteger n / fromInteger (10 ^ negate e)
    convert (Object _) = 0/0
    convert (Array _) = 0/0
    convert Null = 0/0
    convert v = def v


instance Convert Float where
    convert (Number n e)
        | e >= 0    = fromInteger (n * 10 ^ e) / 1.0
        | otherwise = fromInteger n / fromInteger (10 ^ negate e)
    convert (Object _) = 0/0
    convert (Array _) = 0/0
    convert Null = 0/0
    convert v = def v


instance forall a. (Read a, Integral a) => Convert (Ratio a) where
    convert (Number n e)
        | e >= 0    = n' * 10 ^ e' % 1
        | otherwise = n' % 10 ^ negate e'
            where n' = fromInteger n :: a
                  e' = fromInteger e :: a
    convert v = def v % 1


instance Convert Integer where
    convert (Number n e) = int n e
    convert v = def v

    toJSON = flip Number 1


instance Convert Int where
    convert (Number n e) = fromInteger $ int n e
    convert v = def v

    toJSON = flip Number 1 . toInteger


instance Convert Int8 where
    convert (Number n e) = fromInteger $ int n e
    convert v = def v

    toJSON = flip Number 1 . toInteger


instance Convert Int16 where
    convert (Number n e) = fromInteger $ int n e
    convert v = def v

    toJSON = flip Number 1 . toInteger


instance Convert Int32 where
    convert (Number n e) = fromInteger $ int n e
    convert v = def v

    toJSON = flip Number 1 . toInteger

    
instance Convert Int64 where
    convert (Number n e) = fromInteger $ int n e
    convert v = def v

    toJSON = flip Number 1 . toInteger


int :: Integral a => a -> a -> a
int n e
    | e >= 0    = n * 10 ^ e
    | otherwise = n `quot` 10 ^ negate e

def :: (Read a, Num a) => Value -> a
def True  = 1
def False = 0
def Null  = 0
def (String s) = let v = reads s in case v of ((n, _):_) -> n; _ -> 0
def (Array _)  = 0
def (Object _) = 0
def _ = undefined


