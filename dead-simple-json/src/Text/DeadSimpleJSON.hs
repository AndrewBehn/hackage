{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall -O2
    -fno-warn-unused-do-bind
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
    -fno-warn-orphans #-}

{- |
Module      :  DeadSimpleJSON.hs
Copyright   :  (c) Julian Fleischer
License     :  MIT

Maintainer  :  Julian Fleischer <julian.fleischer@fu-berlin.de>
Stability   :  experimental
Portability :  portable

A simple approach for parsing JSON.

To read JSON data use 'read'. To print JSON data use 'show':

> let jsonData = read "[1,2,4,8,16]" :: JSON
> putStrLn $ show jsonData

You can query json data using '?'. Querying implies conversion,
therefor you may need to specify the result type:

> let jsonData = read "{\"seven\": 7, \"nine\": [1,2,4,8,16]}"
> print $ (jsonData ? "nine[3]" :: Int)

For tighter control use 'parse'. A more convenient way for
creating JSON objects in source code or querying JSON data,
is using Template Haskell. See @Text.SimpleJSON.TH@.

The recommended way for importing this module is importing
it qualified, like so:

> import qualified Text.SimpleJSON as JSON
> import Text.SimpleJSON (JSON)

-}
module Text.DeadSimpleJSON (
    -- * Parsing strings
    parse,
    parse',
    parseM,

    -- * Basic data types
    Value (..),
    JSON,

    -- ** Querying json data
    (?),
    top,

    -- ** Conversion to and from
    Convert (..)
) where


import Prelude hiding (True, False)
import qualified Prelude

import Data.Char (isControl)

import qualified Data.Map as M
import qualified Data.Vector as V

import Text.Parsec hiding (parse)
import Control.Applicative ((*>), (<*))
import Data.Functor.Identity

import Numeric (showHex, readHex)

import Text.DeadSimpleJSON.Convert
import Text.DeadSimpleJSON.Query
import Text.DeadSimpleJSON.Types


instance Show JSON where
    show (JSON jsonObject) = write jsonObject

instance Read JSON where
    readsPrec _ =
        either (const []) (:[]) . runIdentity . runParserT json () "-"


parse :: String -> Either ParseError JSON
-- ^ Parse a String for JSON data or return a ParseError.
parse str = runIdentity $ runParserT json' () "-" str

parse' :: String -> Maybe Value
-- ^ Parses a top-level JSON object, returning Just a Value or Nothing.
parse' = parseM

parseM :: Monad m => String -> m Value
-- ^ Purely Monadic version of 'parse''.
parseM = either (fail . show) (return . top) . parse

top :: JSON -> Value
-- ^ Unwraps a top-level JSON object to a Value.
top (JSON v) = v


write x = case x of
    (Number n e) -> writeNumber n e
    (String s) -> '"' : writeString s
    (Object m) -> writeObject m
    (Array v)  -> writeArray v
    True  -> "true"
    False -> "false"
    Null  -> "null"

writeNumber n 0 = show n
writeNumber n e = show n ++ "e" ++ show e

writeString (x:xs)
    | isControl x   = "\\u"  ++ showHex (fromEnum x) (writeString xs)
    | x == '\x2028' = "\\u2028"
    | x == '\x2029' = "\\u2029"
    | x == '\\'     = "\\\\" ++ writeString xs
    | x == '\"'     = "\\\"" ++ writeString xs
    | otherwise     = x : writeString xs
writeString [] = "\""

writeArray arr
    | V.null arr = "[]"
    | otherwise  = '[' : (tail $ concat $ V.foldr (\v l -> "," : write v : l) ["]"] arr)
-- note that using V.foldr' instead of V.foldr makes the SpecConstr optimizer go nuts (-fspec-constr)

writeObject obj
    | M.null obj = "{}"
    | otherwise  = '{' : (tail $ concat $ M.foldrWithKey' (\k v l -> ",\"" : writeString k : ":" : write v : l) ["}"] obj)


json' :: Monad m => ParsecT String () m JSON
json  :: Monad m => ParsecT String () m (JSON, String)

value, jsonString, number, negativeNumber, nonNegativeNumber, object, array
    :: Monad m => ParsecT String () m Value

stringChar, escapedChar
    :: Monad m => ParsecT String () m Char

keyValue
    :: Monad m => ParsecT String () m (String, Value)

mkNumber
    :: Monad m => String -> String -> String -> ParsecT String u m Value


json = do
    val  <- spaces *> (object <|> array)
    rest <- many anyToken
    return (JSON val, rest)

json' = do
    val <- spaces >> (object <|> array) <* spaces
    (char '\EOT' >> return ()) <|> eof
    return $ JSON val


value = jsonString
    <|> number
    <|> object
    <|> array
    <|> (string "true"  >> return True)
    <|> (string "false" >> return False)
    <|> (string "null"  >> return Null)

jsonString = do
    char '"'
    str <- many (stringChar <|> escapedChar)
    char '"'
    return $ String str

stringChar = satisfy (\x -> not (isControl x || (elem x "\\\"")))

escapedChar = do
    char '\\'
    oneOf "\"\\/"
      <|> (oneOf "bfnrt" >>= special)
      <|> (char 'u' >> count 4 hexDigit >>= convert)
      <?> "escape sequence: one of b, f, n, r, t, or uXXXX"
        where
            special chr = return $ case chr of
                'b' -> '\b'
                'f' -> '\f'
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                _   -> undefined
            convert str = do
                let [(hex, _)] = readHex str
                return $ toEnum hex

number = negativeNumber <|> nonNegativeNumber

negativeNumber = do
    char '-'
    (Number nom denom) <- nonNegativeNumber
    return $ Number (negate nom) denom

nonNegativeNumber = do
    num <- digits <|> string "0"
    mantisse <- option "" (char '.' *> many1 digit)
    exp <- option "0" exponent
    mkNumber num mantisse exp
        where
            digits = do
                first <- oneOf ['1'..'9']
                rest  <- many digit
                return $ first : rest
            exponent = do
                oneOf "eE"
                sign <- option '+' $ oneOf "+-"
                exp  <- many1 digit
                return $ sign : exp

mkNumber str1 str2 ('+':str3) = mkNumber str1 str2 str3
mkNumber str1 str2 str3 = do
    let str2' = reverse $ dropWhile (== '0') $ reverse str2
        [(nom, _)] = reads (str1 ++ str2')
        [(exp, _)] = reads (str3)
        exp'   = negate (fromIntegral (length str2') + negate exp)
        num n d
            | n == 0 = Number 0 0
            | n `rem` 10 == 0 = num (n `quot` 10) (d+1)
            | otherwise = Number n d
    return $ num nom exp'

object = do
    pairs <- between (char '{') (char '}') $ do
        spaces
        sepBy keyValue (char ',' >> spaces)
    return $ Object $ M.fromList pairs

keyValue = do
    (String key) <- jsonString <* char ':'
    val <- between spaces spaces value
    return (key, val)

array = do
    values <- between (char '[') (char ']') $ do
        spaces
        sepBy (value <* spaces) (char ',' >> spaces)
    return $ Array $ V.fromList values


