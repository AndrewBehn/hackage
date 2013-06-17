{-# LANGUAGE Haskell2010, TemplateHaskell, TypeOperators #-}

import Data.Name
import Data.NamedRecord


record "TX"
    `has` "thing" := ''String
--    `has` "thing" := ''Int

name "thing"

type XX  = T :& H :& I :& N :& G := String

