{-# LANGUAGE Haskell2010, TypeOperators #-}

import Data.Name
import Data.NamedRecord


firstName = __ :: F :& I :& R :& S :& T  :& N_ :& A :& M :& E
lastName  = __ :: L :& A :& S :& T       :& N_ :& A :& M :& E

type Person =
       F :& I :& R :& S :& T  :& N_ :& A :& M :& E := String
    :+ L :& A :& S :& T       :& N_ :& A :& M :& E := String

displayName obj = print $ (obj `get` firstName) ++ " " ++ (obj `get` lastName)

main = do
    let user = (new :: Person)
            `set` firstName := "Alexander"
            `set` lastName  := "Carnicero"

    displayName user



