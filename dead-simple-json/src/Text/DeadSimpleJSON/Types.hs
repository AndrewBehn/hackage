{-# LANGUAGE Haskell2010,
             DeriveDataTypeable #-}
{-# OPTIONS -Wall -O2 #-}

-- | The basic JSON data types.
module Text.DeadSimpleJSON.Types (
    Value (..),
    JSON (..)
) where


import Data.Data
import Data.Map (Map)
import Data.Vector (Vector)


-- | A top-level JSON object.
--
-- Merely a wrapper that ensures that no other 'Value's
-- but 'Array' and 'Object' reside at the top-level.
newtype JSON = JSON Value
    deriving (Eq, Data, Typeable)

-- | A JSON value.
data Value

    = String String
    -- ^ A JSON String, represented as ordinary Haskell String.

    | Number !Integer !Integer
    -- ^ A JSON Number, represented by two 'Integer's in
    -- exponontial form. @Number n e@ is the same as @{n}e{exp}@,
    -- that is @n * 10 ^ e@. This allows for arbitrary precision
    -- fixed point rationals. See 'Convert' for easy conversions.

    | Object (Map String Value)
    -- ^ A JSON Object, represented as 'Map'.

    | Array  (Vector Value)
    -- ^ A JSON Array, represented as 'Vector' of Values.

    | True
    -- ^ True.

    | False
    -- ^ False.

    | Null
    -- ^ Null (void, unit, @()@).

    deriving (Eq, Show, Read, Data, Typeable)


