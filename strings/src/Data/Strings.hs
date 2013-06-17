{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             Trustworthy #-}

-- |
-- Copyright: Julian Fleischer
-- License: MIT
--
-- Maintainer: Julian Fleischer <julian.fleischer@fu-berlin.de>
-- Stability: provisional
-- Portability: portable
--
-- Functions for working with strings, including 'Text', 'ByteString', etc.
--
-- This module aims at offering a consistent interface across all the available
-- string types. It currently offers instances for the ordinary Haskell 'String'
-- type, 'Text', lazy 'LazyT.Text', 'ByteString', and lazy 'LazyB.ByteString'.
--
-- It especially provides functions for some types, which are otherwise not
-- available nativly (such as 'breakOnSubstring' which is not available for the
-- lazy 'LazyT.Text' type, is offered by 'sBreak' and 'strBreak').
module Data.Strings (

    text,
    lazyText,
    bytes,
    lazyBytes,

    charToByte,
    byteToChar,

    Str (..),
    Strings (..)

) where

import Data.Int
import Data.Word
import Data.String hiding (fromString)
import qualified Data.String as S
import qualified Data.Char as C

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyT
import qualified Data.List as L
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.Encoding as LazyEnc

-- | Returns @True@ exactly for space (0x20), and horizontal tab (0x09).
isSpace x
    | x == ' '  = True
    | x == '\t' = True
    | otherwise = False

-- | Returns @True@ exactly for space (0x20), horizontal tab (0x09), carriage return (0x0D), and line feed (0x0A).
isWhiteSpace x
    | x == ' '  = True
    | x == '\t' = True
    | x == '\r' = True
    | x == '\n' = True
    | otherwise = False

charToByte :: Char -> Word8
-- ^ Convert a 'Char' into a 'Word8'.
charToByte = fromIntegral . C.ord

byteToChar :: Word8 -> Char
-- ^ Convert a 'Word8' into a 'Char'.
byteToChar = C.chr . fromIntegral

text :: String -> Text
-- ^ Create a 'Text' object form an ordinary Haskell 'String'.
text = S.fromString

lazyText :: String -> LazyT.Text
-- ^ Create a lazy 'LazyT.Text' object from an ordinary Haskell 'String'.
lazyText = S.fromString

bytes :: String -> ByteString
-- ^ Create a 'ByteString' object form an ordinary Haskell 'String'.
-- This function will encode a String using the UTF-8 character encoding.
bytes = sFromUnicodeString

-- | Create a lazy 'LazyB.ByteString' object from an ordinary Haskell 'String'.
-- This function will encode a String using the UTF-8 character encoding.
lazyBytes :: String -> LazyB.ByteString
lazyBytes = sFromUnicodeString

-- | The 'Str' class provides functions for working with arbitrary Strings.
-- It is basically the same interface as provided by the 'Strings' class.
-- However, every input string is a Haskell String here, thus easing the
-- usage of different string types with native Haskell String literals.
--
-- For example @strAppend "suffix"@ works with any string type for which
-- an instance of @Str@ is defined. In order to maximize the ease of use
-- of this library, the functions are prefixed with @str@.
--
-- The complexity and efficiency of these functions depends on the underlying
-- string type being used.
--
-- Minimal complete definition: It suffices to provide instances for
-- 'Eq' and 'Strings'.
class (Eq a, Strings a) => Str a where

    -- | Check whether the given string is empty or not. 'null' generalised.
    strNull :: a -> Bool

    -- | 'length' generalised.
    strLen :: a -> Int

    -- | 'head' generalised. This function is undefined if 'strNull' would return @True@.
    strHead :: a -> Char

    -- | 'last' generalised. This function is undefined if 'strNull' would return @True@.
    strLast :: a -> Char

    -- | 'init' generalised. This function is undefined if 'strNull' would return @True@.
    strInit :: a -> a

    -- | 'tail' generalised. This function is undefined if 'strNull' would return @True@.
    strTail :: a -> a

    -- | 'take' generalised.
    strTake :: Int -> a -> a

    -- | 'drop' generalised.
    strDrop :: Int -> a -> a
    
    -- | Replace a substring with another string.
    strReplace :: String -- ^ Needle.
               -> String -- ^ Replacement.
               -> a      -- ^ Haystack.
               -> a      -- ^ Result: @Haystack@ with @Needle@ replaced by @Replacement@.

    -- | Breaks the string on the first occurence of the given substring.
    --
    -- > strBreak "xx" "1x2xx3xx4" = ("1x2", "xx3xx4")
    strBreak :: String -> a -> (a, a)

    -- | Like 'strBreak', but the string to break on is excluded from the result.
    --
    -- > strSplit "xx" "1x2xx3xx4" = ("1x2", "3xx4")
    strSplit :: String -> a -> (a, a)

    -- | Split a string into multiple fragments, separated by the given substring.
    --
    -- > strSplitAll "xx" "1x2xx3xx4" = ["1x2", "3", "4"]
    strSplitAll :: String -> a -> [a]

    -- | Turn all characters in the string to upper case.
    strToUpper :: a -> a
    
    -- | Turn all characters in the string to lower case.
    strToLower :: a -> a

    -- | Turn the first character in the string to upper case.
    strCapitalize :: a -> a

    -- | @map@ generalised.
    strMap :: (Char -> Char) -> a -> a

    -- | @concat@ generalised.
    strConcat :: [a] -> a

    -- | Glue together multiple strings by a given Haskell 'String'.
    --
    -- > strJoin x = concat . intersperse x
    strJoin :: String -> [a] -> a

    -- | Appends the given Haskell 'String' to the string. @++@ generalised.
    --
    -- > strAppend " world" "hello" = "hello world"
    strAppend :: String -- The String to append.
              -> a      -- The string to append to.
              -> a      -- The concatenation of the two strings.

    -- | Cons generalised.
    strCons :: Char -> a -> a

    -- | Strips white space characters off both ends of the string.
    strTrim :: a -> a

    -- | Appends the given character n times to the left, such that
    -- the resulting string has the given length.
    --
    -- > strPadLeft '0' 8 "4711" == "00004711"
    strPadLeft  :: Char -> Int -> a -> a

    -- | Appends the given character n times to the right, such that
    -- the resulting string has the given length.
    --
    -- > strPadRight '0' 8 "4711" == "47110000"
    strPadRight :: Char -> Int -> a -> a

    -- | Appends the given character n times to both sides, such that
    -- the resulting string has the given length.
    --
    -- > strPadBoth '0' 8 "4711" == "00471100"
    strPadBoth  :: Char -> Int -> a -> a

    -- | Reverse the string.
    strReverse :: a -> a

    -- | Check if the given Haskell String equals the string.
    strEq :: String -- ^ The Haskell String.
          -> a      -- ^ The string.
          -> Bool   -- @True@ iff @fromString haskellString == string@

    -- | Check if the string starts with the given Haskell String.
    strStartsWith :: a -> String -> Bool

    -- | Check if the string ends with the given Haskell String.
    strEndsWith :: a -> String -> Bool

    -- | Create a string from a Haskell String.
    fromString :: String -> a

    -- | Create a string from a Haskell String. If the string does not support unicode,
    -- the Haskell String is encoded using UTF-8.
    fromUnicodeString :: String -> a

    -- | Convert the string into a Haskell String.
    toString :: a -> String

    -- | Convert the string into a list of bytes.
    toWord8 :: a -> [Word8]

    strNull = sNull
    strLen = sLen

    strHead = sHead
    strLast = sLast

    strInit = sInit
    strTail = sTail
    strTake = sTake
    strDrop = sDrop

    strReplace n r = sReplace (fromString n) (fromString r)

    strBreak n = sBreak (fromString n)
    strSplit d s = (a, b)
      where (a, b, _) = sSplit (fromString d) s
    strSplitAll n = sSplitAll (fromString n)

    strToLower = strMap C.toLower
    strToUpper = strMap C.toUpper
    strCapitalize str = strCons (C.toUpper (strHead str)) (strTail str)

    strCons = sCons
    strMap = sMap

    strConcat = sConcat
    strJoin d = let d' = fromUnicodeString d in strConcat . L.intersperse d'
    strAppend x xs = sConcat [xs, fromString x]
    strTrim = sTrim

    strPadLeft  = sPadLeft
    strPadRight = sPadRight
    strPadBoth  = sPadBoth

    strReverse = sReverse

    strStartsWith s pref = sStartsWith s (fromString pref)
    strEndsWith   s suff = sEndsWith   s (fromString suff)

    strEq s1 s2 = fromUnicodeString s1 == s2

    fromString = sFromString
    fromUnicodeString = sFromUnicodeString

    toString = sToString
    toWord8 = sToWord8

{-
instance IsString Data.ByteString.ByteString where
    fromString = Enc.encodeUtf8 . Data.Text.pack

instance IsString Data.ByteString.Lazy.ByteString where
    fromString = LazyEnc.encodeUtf8 . Data.Text.Lazy.pack
-}

instance Str Text where
instance Str LazyT.Text where
instance Str ByteString where
instance Str LazyB.ByteString where
instance Str String where

-- | The goal of this class is to offer the same interface for various
-- types of strings ('ByteString', 'Text', Haskell 'String', etc.).
-- If a certain type offers a native implementation for a given function,
-- 'Strings' uses it. If not, a default implementation is given.
--
-- All of these functions are prefixed with @s@ to avoid nameclashes
-- with existing functions from the prelude.
--
-- The complexity and efficiency of these functions depends on the underlying
-- string type being used.
class Strings a where

    -- | Check whether the given string is empty or not. @null@ generalised.
    sNull :: a -> Bool

    -- | The empty string.
    sEmpty :: a

    -- | 'length' generalised.
    sLen :: a -> Int

    -- | 'head' generalised. This function is undefined if 'strNull' would return @True@.
    sHead :: a -> Char

    -- | 'last' generalised. This function is undefined if 'strNull' would return @True@.
    sLast :: a -> Char

    -- | 'init' generalised. This function is undefined if 'strNull' would return @True@.
    sInit :: a -> a

    -- | 'tail' generalised. This function is undefined if 'strNull' would return @True@.
    sTail :: a -> a

    -- | 'take' generalised.
    sTake :: Int -> a -> a

    -- | 'drop' generalised.
    sDrop :: Int -> a -> a

    -- | 'takeWhile' generalised.
    sTakeWhile :: (Char -> Bool) -> a -> a

    -- | 'dropWhile' generalised.
    sDropWhile :: (Char -> Bool) -> a -> a

    -- | Replace a substring with another string.
    sReplace :: a -> a -> a -> a

    -- | Breaks the string on the first occurence of the given substring.
    --
    -- > strBreak "xx" "1x2xx3xx4" = ("1x2", "xx3xx4")
    sBreak :: a -> a -> (a, a)

    -- | Like 'sBreak', but the string to break on is excluded from the result.
    --
    -- > strSplit "xx" "1x2xx3xx4" = ("1x2", "3xx4")
    sSplit :: a -> a -> (a, a, Bool)

    -- | Split a string into multiple fragments, separated by the given substring.
    --
    -- > strSplitAll "xx" "1x2xx3xx4" = ["1x2", "3", "4"]
    sSplitAll :: a -> a -> [a]

    -- | Check if the string starts with the given string.
    sStartsWith :: a -> a -> Bool

    -- | Check if the string ends with the given string.
    sEndsWith   :: a -> a -> Bool

    -- | Cons
    sCons :: Char -> a -> a
    
    -- | Turn the first character into an upper case character.
    sCapitalize :: a -> a

    -- | Map a function over all characters.
    sMap :: (Char -> Char) -> a -> a

    -- | Concatenate all the strings in the list to a single string.
    sConcat :: [a] -> a

    -- | Strips white space characters off both ends of the string.
    sTrim :: a -> a

    -- | Appends the given character n times to the left, such that
    -- the resulting string has the given length.
    --
    -- > strPadLeft '0' 8 "4711" == "00004711"
    sPadLeft  :: Char -> Int -> a -> a

    -- | Appends the given character n times to the right, such that
    -- the resulting string has the given length.
    --
    -- > strPadRight '0' 8 "4711" == "47110000"
    sPadRight :: Char -> Int -> a -> a

    -- | Appends the given character n times to both sides, such that
    -- the resulting string has the given length.
    --
    -- > strPadBoth '0' 8 "4711" == "00471100"
    sPadBoth  :: Char -> Int -> a -> a

    -- 'reverse' the string.
    sReverse :: a -> a

    -- | Create a string from a Haskell String.
    sFromString :: String -> a

    -- | Create a string from a Haskell String. If the string does not support unicode,
    -- the Haskell String is encoded using UTF-8.
    sFromUnicodeString :: String -> a

    -- | Convert the string into a Haskell String.
    sToString :: a -> String

    -- | Convert the string into a list of bytes.
    sToWord8 :: a -> [Word8]

    sCapitalize xs
        | sNull xs  = xs
        | otherwise = sCons (C.toUpper (sHead xs)) (sTail xs)

    sBreak d src = search 0 src
      where
        search a b | a `seq` b `seq` False = undefined
        search n s
            | sNull s         = (src, sEmpty)
            | sStartsWith s d = (sTake n src, s)
            | otherwise       = search (n+1) (sTail s)

    sReplace n r = sConcat . replace n r
      where
        replace n r h = if sNull b
                        then (if c then [a, r] else [a])
                        else a : r : replace n r b
          where (a, b, c) = sSplit n h

    sSplit d s = (a, sDrop (sLen d) b, not $ sNull b)
      where (a, b) = sBreak d s

    sSplitAll d s = if sNull b
                      then (if c then [a, b] else [a])
                      else a : sSplitAll d b
      where (a, b, c) = sSplit d s

    sPadLeft  c n s = let len    = sLen s
                          padLen = max 0 (n - len)
                          padStr = sFromString $ replicate padLen c
                      in  sConcat [padStr, s]
                      
    sPadRight c n s = let len    = sLen s
                          padLen = max 0 (n - len)
                          padStr = sFromString $ replicate padLen c
                      in  sConcat [s, padStr]      

    sPadBoth  c n s = let len     = sLen s
                          padLen  = (max 0 (n - len))
                          padLenR = padLen `quot` 2
                          padLenL = padLen - padLenR
                          padStrL = sFromString $ replicate padLenL c
                          padStrR = sFromString $ replicate padLenR c
                      in  sConcat [padStrL, s, padStrR]

    sFromString = sFromUnicodeString

    sToWord8 = sToWord8 . sToString


instance Strings ByteString where

    sNull  = B.null
    sEmpty = B.empty

    sHead = toEnum . fromIntegral . B.head
    sLast = toEnum . fromIntegral . B.last

    sInit = B.init
    sTail = B.tail

    sTake = B.take
    sTakeWhile f = B.takeWhile (f . chr)
    sDrop = B.drop
    sDropWhile f = B.dropWhile (f . chr)

    sBreak = B.breakSubstring

    sStartsWith = flip B.isPrefixOf
    sEndsWith = flip B.isSuffixOf

    sCons = B.cons . charToByte
    sMap f = B.map (charToByte . f . byteToChar)
    sConcat = B.concat
    sLen = B.length

    sTrim s = let (a, b) = B.span    (isWhiteSpace . chr) s
                  (c, d) = B.spanEnd (isWhiteSpace . chr) b
              in c

    sReverse = B.reverse

    sFromString = B8.pack
    sFromUnicodeString = Enc.encodeUtf8 . T.pack

    sToString = B8.unpack
    sToWord8 = B.unpack


instance Strings LazyB.ByteString where

    sNull  = LazyB.null
    sEmpty = LazyB.empty

    sHead = toEnum . fromIntegral . LazyB.head
    sLast = toEnum . fromIntegral . LazyB.last

    sInit = LazyB.init
    sTail = LazyB.tail

    sTake = LazyB.take . fromIntegral
    sTakeWhile f = LazyB.takeWhile (f . chr)
    sDrop = LazyB.drop . fromIntegral
    sDropWhile f = LazyB.dropWhile (f . chr)

    sStartsWith = flip LazyB.isPrefixOf
    sEndsWith = flip LazyB.isSuffixOf

    sCons = LazyB.cons . charToByte
    sMap f = LazyB.map (charToByte . f . byteToChar)
    sConcat = LazyB.concat
    sLen = fromIntegral . LazyB.length

    sTrim = LazyB.fromChunks . (:[]) . sTrim . B.concat . LazyB.toChunks

    sReverse = LazyB.reverse

    sFromString = LazyB8.pack
    sFromUnicodeString = LazyEnc.encodeUtf8 . LazyT.pack

    sToString = LazyB8.unpack
    sToWord8 = LazyB.unpack


instance Strings Text where

    sNull  = T.null
    sEmpty = T.empty

    sHead = T.head
    sLast = T.last

    sInit = T.init
    sTail = T.tail

    sTake = T.take
    sTakeWhile = T.takeWhile
    sDrop = T.drop
    sDropWhile = T.dropWhile

    sReplace = T.replace
    sBreak = T.breakOn

    sStartsWith = flip T.isPrefixOf
    sEndsWith = flip T.isSuffixOf

    sCons = T.cons
    sMap = T.map
    sConcat = T.concat
    sLen = T.length

    sTrim = T.strip

    sPadLeft  = flip T.justifyLeft
    sPadRight = flip T.justifyRight
    sPadBoth  = flip T.center

    sReverse = T.reverse

    sFromUnicodeString = S.fromString
    sFromString = S.fromString

    sToString = T.unpack
    sToWord8 = B.unpack . Enc.encodeUtf8


instance Strings LazyT.Text where

    sNull  = LazyT.null
    sEmpty = LazyT.empty

    sHead = LazyT.head
    sLast = LazyT.last

    sInit = LazyT.init
    sTail = LazyT.tail

    sTake = LazyT.take . fromIntegral
    sTakeWhile = LazyT.takeWhile
    sDrop = LazyT.drop . fromIntegral
    sDropWhile = LazyT.dropWhile

    sBreak = LazyT.breakOn

    sStartsWith = flip LazyT.isPrefixOf
    sEndsWith = flip LazyT.isSuffixOf

    sCons = LazyT.cons
    sMap = LazyT.map
    sConcat = LazyT.concat
    sLen = fromIntegral . LazyT.length

    sTrim = LazyT.strip

    sPadLeft  = flip (LazyT.justifyLeft . fromIntegral)
    sPadRight = flip (LazyT.justifyRight . fromIntegral)
    sPadBoth  = flip (LazyT.center . fromIntegral)

    sReverse = LazyT.reverse

    sFromUnicodeString = S.fromString
    sFromString = S.fromString

    sToString = LazyT.unpack
    sToWord8 = LazyB.unpack . LazyEnc.encodeUtf8


instance Strings String where

    sNull = null
    sEmpty = []

    sHead = head
    sLast = last

    sInit = init
    sTail = tail

    sTake = take
    sTakeWhile = takeWhile
    sDrop = drop
    sDropWhile = dropWhile

    sStartsWith = flip L.isPrefixOf
    sEndsWith = flip L.isSuffixOf

    sCapitalize (x:xs) = C.toUpper x : xs
    sCapitalize _ = []

    sCons = (:)
    sMap = map

    sConcat = concat
    sLen = length

    sTrim = T.unpack . T.strip . T.pack

    sReverse = reverse

    sFromString = id
    sFromUnicodeString = id

    sToString = id
    sToWord8 = B.unpack . Enc.encodeUtf8 . T.pack

-- | Utility class for working with characters.
class Chars a where
    -- | Retrieve the value of the given character.
    ord :: Char -> a

    -- | Construct a character form the given value.
    chr :: a -> Char

instance Chars Int where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Word8 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Word16 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Word32 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Word64 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Int8 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Int16 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Int32 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Int64 where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral

instance Chars Integer where
    ord = fromIntegral . fromEnum
    chr = toEnum . fromIntegral


