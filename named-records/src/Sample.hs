{-# LANGUAGE Haskell2010, TemplateHaskell #-}

module Sample where

import Data.NamedRecord
import Data.Word

names
    "firstName"
    "lastName"
    "secondName"
    "id"

id_ = Sample.id

record' "Options"
    `has` "optSomething" := True

record "Person"
    `has` "id"        := ''Word64

    `has` "firstName" := ''String
    `has` "lastName"  := ''String

julian = (new :: Person)
    `set` id_ := (2 ^ 64 - 1)
    `set` firstName := "Julian"
    `set` lastName := "Fleischer"

incrementId x = x `upd` id_ += 1

a +=  b = a := (+ b)
a -=  b = a := (- b)
a *=  b = a := (* b)
a /=  b = a := (/ b)
a //= b = a := (`div` b)

a ++= b = a := (++ b)
a &&= b = a := (&& b)
a ||= b = a := (|| b)











