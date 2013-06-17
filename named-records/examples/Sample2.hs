{-# LANGUAGE Haskell2010, TemplateHaskell #-}

module Sample2 where

import Data.NamedRecord
import Data.Word


record "Account"
    `has` "id"        := ''Word64

    `has` "loginName" := ''String
    `has` "password"  := ''String


record "Person"
    `has` "id"        := ''Word64

    `has` "firstName" := ''String
    `has` "lastName"  := ''String


