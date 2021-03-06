{-# LANGUAGE Haskell2010
    , TemplateHaskell
    , MultiParamTypeClasses
    , FunctionalDependencies
    , TypeOperators
    , FlexibleInstances
    , UndecidableInstances
    , OverlappingInstances
 #-}

{- | Flexible records with named fields.

Named records allow you to define und use records
with labeled fields. These records are first class
objects. Record fields are labeled by names, which
can basically be any type. However, the names package
provides global name types and some syntactic sugar
to use them.

Here is a complete walk-through, with Template Haskell
syntactic sugar.

This is how a typical example preamble looks like:

> import Data.NamedRecord

In order to use names you need to declare them first
(see the @names@ package for further details):

> name "firstName"
> name "lastName"

These are two records @Person@ and @User@:

> record "Person"
>     `has` "firstName" := ''String
>     `has` "lastName"  := ''String
>
> record "User"
>     `has` "firstName" := ''String
>     `has` "lastName"  := ''String
>     `has` "loginName" := ''String

Note that these declarations create constructor
functions @newPerson@ and @newUser@, as well as
type synonyms @Person@ and @User@ (use @-ddump-splices@
to see what has been generated).

Here are two instances of these recors:

> julian = newPerson
>    `set` firstName := "Julian"
>    `set` lastName  := "Fleischer"
>
> alexander = newUser
>    `set` firstName := "Alexander"
>    `set` lastName  := "Carnicero"
>    `set` loginName := "alexander.carnicero"

We can now create a @displayName@ function like
the following:

> displayName obj =
>     (obj `get` firstName) ++ " " ++
>     (obj `get` lastName)

Note that this function will accept any record
that has a @firstName@ and a @lastName@ field of
type @String@.

>>> displayName julian
Julian Fleischer

>>> displayName alexander
Alexander Carnicero

As mentioned above, records are first class citizens.
That means you can create them anywhere:

>>> displayName (firstName := "John" :+ lastName := "Doe")
John Doe

It is also possible to declare default values:

> name "serverName"
> name "port"
> 
> record "ServerConfig"
>     `has` "serverName" := ''String := "localhost"
>     `has` "port"       := ''Int := (4711 :: Int)

>>> newServerConfig
serverName := "localhost" :+ port := 4711

>>> newServerConfig `set` serverName := "example.org"
serverName := "example.org" :+ port := 4711

>>> newServerConfig `get` port
4711

Complex expressions and types need to be quoted using
@[e| expr |]@ and @[t| type |]@ like so:

> record "Server"
>     `has` "requestHandler" := [t| Request -> Response |]
>                            := [e| \x -> answer x |]
>     `has` "config" := ''Config := [e| newConfig |]

It is furthermore possible to extend existing records
(but due to stage restrictions in GHCs implementation of
Template Haskell, two records of which one extends the other
can not be contained in the same module):

> module Sample2 where
> 
> import Data.NamedRecord
> import Data.Word
> 
> record "Account"
>     `has` "id"        := ''Word64
> 
>     `has` "loginName" := ''String
>     `has` "password"  := ''String
> 
> record "Person"
>     `has` "id"        := ''Word64
> 
>     `has` "firstName" := ''String
>     `has` "lastName"  := ''String

> module Sample where
> 
> import Data.NamedRecord
> import Data.Word
> 
> import Sample2
> 
> record "User"
>     `extends` __Person
>     `extends` __Account
> 
>     `has` "id"           := ''Word64
>     `has` "emailAddress" := ''String

-}
module Data.NamedRecord (
    Property (get, set, upd),
    New (new),
    add,

    (:=) (..),
    (:+) (..),

    -- * Template Haskell Syntactic Sugar

    -- | Declares a record (looks like a new keyword @record@).
    -- See the examples.
    record,

    -- | As 'record', but also generated the accessor names.
    record',

    extends,

    -- | Declares a field of a record. Use as infix operators.
    -- See the examples.
    has,

    -- ** Names
    -- For convenience, this module re-exports name TH name functions.
    name, nameT, nameV, names
) where


import Control.Applicative
import Control.Monad
import Data.Binary hiding (get)
import qualified Data.Binary as B
import Data.Data
import Data.Function (on)
import Data.List
import qualified Data.Name
import Data.Name (name, nameT, nameV, names)
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift (..))


data a := b = a := b deriving Show

infixr 3 :=


data a :+ b = a :+ b deriving Show

infixr 2 :+


add :: b -> a -> a :+ b
add = flip (:+)

infixl 1 `add`


class Property o n v | o n -> v where
    get :: o -> n -> v
    set :: o -> n := v -> o
    upd :: o -> n := (v -> v) -> o

infixl 1 `set`
infixl 1 `get`
infixl 1 `upd`


instance Property (n := v) n v where
    get (_ := v) _ = v
    set _ v = v
    upd (_ := v) (n := f) = (n := f v)

instance Property ((n := v) :+ b) n v where
    get (a :+ b) n = get a n
    set (a :+ b) p = (set a p) :+ b
    upd (a :+ b) f = (upd a f) :+ b

instance Property b n v => Property (a :+ b) n v where
    get (a :+ b) n = get b n
    set (a :+ b) p = a :+ (set b p)
    upd (a :+ b) f = a :+ (upd b f)


instance Binary v => Binary (n := v) where
    put (_ := v) = put v
    get = (_type :=) <$> B.get

instance (Binary v, Binary b) => Binary (v :+ b) where
    put (v :+ b) = put v >> put b
    get = liftM2 (:+) B.get B.get


class New o where
    new :: o

instance New (a := b) where
    new = _type := _value

instance (New a, New b) => New (a :+ b) where
    new = new :+ new


class ToExp a where
    toExp' :: a -> Q Exp

instance ToExp (Q Exp) where
    toExp' = id

instance Lift a => ToExp a where
    toExp' = lift


class Field a where
    toExp :: a -> Maybe (Q Exp)
    toType :: a -> Q Type

    toExp _ = Nothing

instance ToExp e => Field (Q Type := e) where
    toExp (_ := e) = Just $ toExp' e
    toType (v := _) = v

instance ToExp e => Field (Name := e) where
    toExp (_ := e) = Just $ toExp' e
    toType (v := _) = return $ ConT v

instance Field Name where
    toType = return . ConT

instance Field (Q Type) where
    toType = id

instance (Data a, Typeable a) => Field a where
    toExp = Just . dataToExpQ (const Nothing)
    toType e = qType typeName
      where
        typeName = show (typeOf e)

        qType typeName = case typeName of

            -- This is a dirty hack. It would be much better
            -- to resolve all type synonyms to their real types.
            -- This would have to be done in (~>)
            "[Char]" -> return $ ConT ''String

            ('[':xs) -> do
                t <- qType (init xs)
                return $ AppT ListT t

            ('M':'a':'y':'b':'e':' ':xs) -> do
                t <- qType xs
                m <- [t| Maybe |]
                return $ AppT m t

            name -> lookupTypeName name >>= typeFor

        typeFor (Just n) = return $ ConT n
        typeFor Nothing = fail $ "The type \"" ++ typeName
            ++ "\" can not be handled by Data.NamedRecord TH sugar"
            ++ " (you might to import the module where it's from)."


class RecordTemplate a b c | a b -> c where
    (~>) :: a -> b -> c

instance (Field v, Field w) => RecordTemplate
        (String := v)
        (String := w)
        [(String, Q Type, Maybe (Q Exp))] where
    (n := v) ~> (m := w) = [(n, toType v, toExp v),
                            (m, toType w, toExp w)]

instance Field v => RecordTemplate
        (String := v)
        [(String, Q Type, Maybe (Q Exp))]
        [(String, Q Type, Maybe (Q Exp))] where
    (n := v) ~> xs = (n, toType v, toExp v) : xs

instance Field v => RecordTemplate Record (String := v) (Q [Dec]) where
    r ~> (n := v) = r ~> [(n, toType v, toExp v)]

instance RecordTemplate
        Record [(String, Q Type, Maybe (Q Exp))] (Q [Dec]) where

    Record name xs opts ~> fs = do
        let typeD typ = TySynD (mkName name) [] typ

            noValue = VarE '_value

            normalize (n, v, d) = do
                v' <- v
                d' <- maybe (return noValue) id d
                return (n, (v', d'))

        nFields <- mapM normalize fs

        let fs' = sortBy (compare `on` fst) (concat (nFields : xs))

            unify sss@(s:ss) =
                if all id (zipWith ((==) `on` (fst . snd)) ss sss)
                    then return $ select sss
                    else fail $ "Types for the named field \""
                        ++ fst s ++ "\" could not be unified."
                        ++ "\n    The conflicting types are: \n"
                        ++ unlines (map (show . fst . snd) sss)

            select (x:[]) = x
            select (x@(_, (_, d)) : xs)
                | d == noValue = select xs
                | otherwise    = x

            mkDef (n, (v, d)) = do
                name <- nameT n
                return (AppT (AppT (ConT ''(:=)) name) v, d)

        nFields <- mapM unify (groupBy ((==) `on` fst) fs')
        fields  <- mapM mkDef nFields
        rExp    <- dataToExpQ (const Nothing) nFields

        let reflD = ValD (VarP (mkName ("__" ++ name))) (NormalB rExp) []

            syn = foldr (\(x, _) xs -> AppT (AppT (ConT ''(:+)) x) xs)
                        (fst $ last fields) (init fields)

            cName = mkName ("new" ++ name)
            sigD  = SigD cName (ConT (mkName name))
            funcD = ValD (VarP cName) (NormalB funcB) []
            funcB = foldr join (field $ last fields) (init fields)
              where
                join x xs = InfixE (Just $ field x) (ConE '(:+)) (Just xs)
                field (_, x) = InfixE (Just (VarE '_type))
                                      (ConE '(:=))
                                      (Just x)

            declaration = [typeD syn, sigD, funcD, reflD]
        
        if opts
          then do
                names <- mapM Data.Name.name (map fst nFields)
                return (declaration ++ concat names)
          else do
                return declaration


_type = error $ "NamedRecord field type unwrapped!"
    ++ " You should never see this."
    ++ " Srsly, what did you do?"

_value = error "Data.NameRecord.undefined: No value set."

has :: RecordTemplate a b c => a -> b -> c
has = (~>)

infixr 1 ~>
infixr 1 `has`


class RecordExtends a where
    (<:) :: Record -> a -> Record

instance RecordExtends [(String, (Type, Exp))] where
    Record name xs opts <: x = Record name (x:xs) opts

extends :: RecordExtends a => Record -> a -> Record
extends = (<:)

infixl 2 <:
infixl 2 `extends`


data Record = Record String [[(String, (Type, Exp))]] Bool

record :: String -> Record
record name = Record name [] False

record' :: String -> Record
record' name = Record name [] True


-- Playground below

fieldNames :: FieldNames a => a -> [String]
fieldNames = _fieldNames []


class FieldNames a where
    _fieldNames :: [String] -> a -> [String]

instance (Show n, FieldNames r) =>
        FieldNames (n := ns :+ r) where

    _fieldNames xs (n := _ :+ r) = _fieldNames (show n : xs) r

instance (Show n) => FieldNames (n := ns) where

    _fieldNames xs (n := _) = show n : xs


