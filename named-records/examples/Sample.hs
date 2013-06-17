{-# LANGUAGE Haskell2010
    , TemplateHaskell
    , FlexibleContexts
    , TypeOperators
    , Trustworthy
 #-}
-- {-# OPTIONS -ddump-splices #-}

module Main where

import Data.NamedRecord
import Data.Word


import Sample2

name "firstName"
name "lastName"
name "loginName"
name "password"


record "User"
    `extends` __Person
    `extends` __Account

    `has` "id"           := ''Word64
    `has` "emailAddress" := ''String


julian = newPerson `set` firstName := "Julian"
                   `set` lastName  := "Fleischer"

alexander = newUser `set` firstName := "Alexander"
                    `set` lastName  := "Carnicero"
                    `set` loginName := "alexander.carnicero"

displayName obj = (obj `get` firstName) ++ " " ++ (obj `get` lastName)

something :: (Property o $(nameT "firstName") String,
              Property o $(nameT "lastName")  String)
          => o -> String
something = displayName

