{-# LANGUAGE Haskell2010,
             TemplateHaskell #-}
{-# OPTIONS -Wall -O2 -fno-warn-name-shadowing #-}

-- | Template Haskell syntax sugar for working with 'JSON' data.
-- 
-- For using this module, you need to declare a LANGUAGE
-- pragma like the following:
-- 
-- > {-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}
module Text.DeadSimpleJSON.TH (

    -- * Query JSON objects
    jsq,

    -- * Create JSON objects
    json,
    jsonF,

    -- * Include strings
    s,
    sF
) where

import Prelude hiding (True, False)
import qualified Prelude

import Data.Char
import qualified Data.Map as M
import qualified Data.Vector as V

import Text.DeadSimpleJSON (parse)
import Text.DeadSimpleJSON.Convert (Convert (..))
import Text.DeadSimpleJSON.Query
import Text.DeadSimpleJSON.Types

import Language.Haskell.TH
import Language.Haskell.TH.Quote


s :: QuasiQuoter
-- ^ A QuasiQuoter on raw strings.
--
-- The definition is basically:
--
-- > s = QuasiQuoter {
-- >   quoteExp  = return . LitE . StringL
-- > }
s = QuasiQuoter {
    quoteExp  = return . LitE . StringL,

    quotePat  = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a dec)"
}


sF :: QuasiQuoter
-- ^ A QuasiQuoter which includes raw strings from files.
--
-- The following example will include the contents of @file.txt@
-- as a @String@.
--
-- > let str = [sF|file.txt|]
--
-- Note that every character inside the brackets is treated
-- as part of the file name, that is @[sF| file.txt |]@ is not
-- the same as the above example (it will try to find a file which
-- name includes space characters).
sF = quoteFile s

jsonF :: QuasiQuoter
-- ^ A QuasiQuoter which includes JSON data from files.
--
-- The following example will include the contents of @data.json@
-- as 'JSON'.
--
-- > let str = [jsonF|data.json|]
--
-- Note that every character inside the brackets is treated
-- as part of the file name, that is @[jsonF| data.json |]@ is not
-- the same as the above example (it will try to find a file which
-- name includes space characters).

jsonF = quoteFile json

json :: QuasiQuoter
-- ^ A QuasiQuoter which includes JSON data.
--
-- The type of the expression is 'JSON'.
json = QuasiQuoter {
    quoteExp  = jsonQuoter,

    quotePat  = \_ -> fail "illegal json QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal json QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal json QuasiQuote (allowed as expression only, used as a dec)"
}

jsonQuoter :: String -> Q Exp
jsonQuoter = either (fail . show) buildJSON . parse
    where
        buildJSON (JSON json) = do
            json' <- buildJSON' json
            return (AppE (ConE 'JSON) json')

        buildJSON' (String s) = return $ AppE (ConE 'String) (LitE (StringL s))
        buildJSON' (Number n e) = return $ AppE (AppE (ConE 'Number) (LitE (IntegerL n))) (LitE (IntegerL e))
        buildJSON' (Object obj) = do
            m <- mapM (\(k, v) -> do { x <- buildJSON' v; return $ TupE [LitE (StringL k), x] }) (M.toList obj)
            return $ AppE (ConE 'Object) (AppE (VarE 'M.fromList) (ListE m))
        buildJSON' (Array arr) = do
            v <- mapM buildJSON' (V.toList arr)
            return $ AppE (ConE 'Array) (AppE (VarE 'V.fromList) (ListE v))
        buildJSON' True = return $ ConE 'True
        buildJSON' False = return $ ConE 'False
        buildJSON' Null = return $ ConE 'Null


jsq :: QuasiQuoter
-- ^ A QuasiQuoter which queries a json object using JavaScript notation.
-- 
-- Suppose obj contains a json object of type JSON:
--
-- > [jsq| obj.prop.list[3] |]
--
-- The above will query the object in obj as if it was JavaScript.
--
-- The type of the expression is polymorphic: @Convert a => a@.
--
-- You will need to specify the type of the query, like so:
--
-- > [jsq| obj.prop.list |] :: [Integer]
--
-- For possible conversions, see the instances for 'Convert'.
jsq = QuasiQuoter {
    quoteExp  = jsqQuoter,

    quotePat  = \_ -> fail "illegal jsq QuasiQuote (allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal jsq QuasiQuote (allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal jsq QuasiQuote (allowed as expression only, used as a dec)"
}

jsqQuoter :: String -> Q Exp
jsqQuoter = either (fail . show) buildQuery . mkQuery' . filter (not . isSpace)
    where
        buildQuery (Field s e) = do
            q <- buildQuery' e
            return $ AppE ((AppE (VarE 'query)) q) (VarE (mkName s))
        buildQuery _ = do
            report Prelude.False "Warning: Empty JSON Query"
            [| convert $ Object M.empty |]

        buildQuery' (Field s e) = do
            exp <- buildQuery' e
            return $ AppE (AppE (ConE 'Field) (LitE (StringL s))) exp
        buildQuery' (Index i e) = do
            exp <- buildQuery' e
            return $ AppE (AppE (ConE 'Index) (LitE (IntegerL (fromIntegral i)))) exp
        buildQuery' (Read) = return $ ConE 'Read


