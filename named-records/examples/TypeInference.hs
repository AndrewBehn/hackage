{-# LANGUAGE Haskell2010, TemplateHaskell #-}

import Data.NamedRecord

names "firstName" "lastName"

record "User"
    `has` "accountId" := ''Integer
    `has` "firstName" := ''String
    `has` "lastName"  := ''String

record "Person"
    `has` "firstName" := ''String
    `has` "lastName"  := ''String


displayName obj = (obj `get` firstName) ++ " " ++ (obj `get` lastName)

