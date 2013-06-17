{-# LANGUAGE Haskell2010, TemplateHaskell #-}

import Data.NamedRecord


record "InputWidget" "a"
    `has` "input" :~ "a"



