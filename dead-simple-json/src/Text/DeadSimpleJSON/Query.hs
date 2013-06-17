{-# LANGUAGE Haskell2010,
             DeriveDataTypeable #-}
{-# OPTIONS -Wall -O2
    -fno-warn-unused-do-bind
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing #-}

module Text.DeadSimpleJSON.Query (
    query,
    query',
    queryV,
    queryV',
    Query (..),
    mkQuery,
    mkQuery',
    (?)
) where


import Text.Parsec

import Text.DeadSimpleJSON.Convert
import Text.DeadSimpleJSON.Types

import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Data
import Data.Functor.Identity
import Control.Monad
import Control.Applicative ((<*))


data Query = Field String Query
           | Index Int Query
           | Read
    deriving (Show, Data, Typeable)


query :: Convert a => Query -> JSON -> a
query q (JSON v) = queryV q v

query' :: Convert a => Query -> JSON -> Maybe a
query' q (JSON v) = queryV' q v


queryV :: Convert a => Query -> Value -> a
queryV (Field f q) (Object m) = maybe (convert Null) (queryV q) (M.lookup f m)
queryV (Index i q) (Array a)  = maybe (convert Null) (queryV q) (a V.!? i)
queryV Read v = convert v

queryV _ _ = convert Null


queryV' :: Convert a => Query -> Value -> Maybe a
queryV' (Field f q) (Object m) = maybe Nothing (queryV' q) (M.lookup f m)
queryV' (Index i q) (Array a)  = maybe Nothing (queryV' q) (a V.!? i)
queryV' Read v = convert' v

queryV' _ _ = Nothing


json ? q = query (mkQuery q) json


type Parser a = ParsecT String () Identity a

ident :: Parser String
ident = liftM2 (:) (letter <|> char '_') (many (alphaNum <|> char '_'))

index :: Parser Int
index = liftM read $ between (char '[') (char ']') (many1 digit)

path :: Parser [Either String Int]
path = liftM concat $ sepBy element (char '.')

element :: Parser [Either String Int]
element = liftM2 (:) (ident >>= return . Left) (many index >>= return . map Right)

mkPath :: String -> [Either String Int]
mkPath = either (const []) id . parse path "-"

mkQuery :: String -> Query
mkQuery = buildQuery . mkPath

mkQuery' :: String -> Either ParseError Query
mkQuery' = either Left (Right . buildQuery) . parse (path <* eof) "JSON Query"

buildQuery = foldr (\x p -> either (\s -> Field s p) (\i -> Index i p) x) Read


