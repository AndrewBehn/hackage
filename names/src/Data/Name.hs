{-# LANGUAGE Haskell2010
    , TemplateHaskell
    , ScopedTypeVariables
    , FlexibleInstances
    , TypeOperators
    , DeriveDataTypeable
    , ScopedTypeVariables
    , Trustworthy
 #-}

{- | Type level names.

Names are like strings on the type level.
This is a name:

> H :& E :& L :& L :& O :& W_ :& O :& R :& L :& D

This package provides types which can be used
as letters and a cons operator (@:&@). It also
provides syntactic sugar for using names via
template haskell:

> name "helloWorld"

This will create a value named @helloWorld@
which has the above type and can be used to
work with the name.

Names are useful for named records. See the
@named-records@ package.

-}
module Data.Name (
    (:&),
    Name (..),
    name, nameT, nameV, names,

    U0, U1, U2, U3, U4, U5, U6, U7, U8, U9,
    UA, UB, UC, UD, UE, UF,

    X0, X1, X2, X3, X4, X5, X6, X7, X8, X9,
    XA, XB, XC, XD, XE, XF,

    D0, D1, D2, D3, D4, D5, D6, D7, D8, D9,

    A, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,

    A_, B_, C_, D_, E_, F_, G_, H_, I_, J_, K_, L_, M_,
    N_, O_, P_, Q_, R_, S_, T_, U_, V_, W_, X_, Y_, Z_,

    __

) where

import Data.Char
import Data.Typeable
import Language.Haskell.TH hiding (Name, Q)
import qualified Language.Haskell.TH as TH
import Numeric

__  = error "Data.Name.undefined: Names are types, not values."

class Show a => Name a where
    nameOf :: a -> String
    nameOf = show

data a :& b deriving Typeable

infixr 4 :&

instance (Show a, Show b) => Show (a :& b) where
    show _ = let showA = show :: a -> String
                 showB = show :: b -> String
             in  showA __ ++ showB __

instance (Name a, Name b) => Name (a :& b)


name :: String -> TH.Q [Dec]
name str = do
    t <- nameT str
    let sig = SigD (mkName str) t
        def = ValD (VarP $ mkName str) (NormalB (VarE '__)) []
    return [sig, def]

class Names' a where
    names' :: [String] -> a

instance Names' (TH.Q [Dec]) where
    names' = fmap concat . mapM name

instance Names' a => Names' (String -> a) where
    names' xs x = names' (x:xs)

names :: Names' a => a
names = names' []

-- FIX: Generate Unicode-Names

nameT :: String -> TH.Q Type
nameT str = return $ foldr f (last names) (init names)
  where
    names = map toName str
    f x xs = AppT (AppT (ConT ''(:&)) x) xs

    conT = ConT . nameFor

    toName c
        | c `elem` ['0'..'9'] = conT ('D' : [c])
        | c `elem` ['a'..'z'] = conT [toUpper c]
        | c `elem` ['A'..'Z'] = conT (c : "_")
        | otherwise =
            let h  = map toUpper . flip showHex ""
                i  = fromEnum c
                u  = conT ('U' : h (i `quot` 16^4))
                xA = conT ('X' : h (i `rem` 16^4 `quot` 16^3))
                xB = conT ('X' : h (i `rem` 16^3 `quot` 16^2))
                xC = conT ('X' : h (i `rem` 16^2 `quot` 16))
                xD = conT ('X' : h (i `rem` 16))
            in foldr AppT xD [u, xA, xB, xC]

nameV :: String -> TH.Q Exp
nameV str = nameT str >>= return . SigE (VarE $ mkName "Data.Name.__")

nameFor str = case str of
    "A" -> ''A ; "B" -> ''B ; "C" -> ''C ; "D" -> ''D ; "E" -> ''E ; "F" -> ''F ;
    "G" -> ''G ; "H" -> ''H ; "I" -> ''I ; "J" -> ''J ; "K" -> ''K ; "L" -> ''L ;
    "M" -> ''M ; "N" -> ''N ; "O" -> ''O ; "P" -> ''P ; "Q" -> ''Q ; "R" -> ''R ;
    "S" -> ''S ; "T" -> ''T ; "U" -> ''U ; "V" -> ''V ; "W" -> ''W ; "X" -> ''X ;
    "Y" -> ''Y ; "Z" -> ''Z

    "A_" -> ''A_ ; "B_" -> ''B_ ; "C_" -> ''C_ ; "D_" -> ''D_ ; "E_" -> ''E_ ; "F_" -> ''F_ ;
    "G_" -> ''G_ ; "H_" -> ''H_ ; "I_" -> ''I_ ; "J_" -> ''J_ ; "K_" -> ''K_ ; "L_" -> ''L_ ;
    "M_" -> ''M_ ; "N_" -> ''N_ ; "O_" -> ''O_ ; "P_" -> ''P_ ; "Q_" -> ''Q_ ; "R_" -> ''R_ ;
    "S_" -> ''S_ ; "T_" -> ''T_ ; "U_" -> ''U_ ; "V_" -> ''V_ ; "W_" -> ''W_ ; "X_" -> ''X_ ;
    "Y_" -> ''Y_ ; "Z_" -> ''Z_

    "D0" -> ''D0 ; "D1" -> ''D1 ; "D2" -> ''D2 ; "D3" -> ''D3 ; "D4" -> ''D4 ;
    "D5" -> ''D5 ; "D6" -> ''D6 ; "D7" -> ''D7 ; "D8" -> ''D8 ; "D9" -> ''D9 ;

    "X0" -> ''X0 ; "X1" -> ''X1 ; "X2" -> ''X2 ; "X3" -> ''X3 ; "X4" -> ''X4 ;
    "X5" -> ''X5 ; "X6" -> ''X6 ; "X7" -> ''X7 ; "X8" -> ''X8 ; "X9" -> ''X9 ;
    "XA" -> ''XA ; "XB" -> ''XB ; "XC" -> ''XC ; "XD" -> ''XD ; "XE" -> ''XE ;
    "XF" -> ''XF ;

    "U0" -> ''U0 ; "U1" -> ''U1 ; "U2" -> ''U2 ; "U3" -> ''U3 ; "U4" -> ''U4 ;
    "U5" -> ''U5 ; "U6" -> ''U6 ; "U7" -> ''U7 ; "U8" -> ''U8 ; "U9" -> ''U9 ;
    "UA" -> ''UA ; "UB" -> ''UB ; "UC" -> ''UC ; "UD" -> ''UD ; "UE" -> ''UE ;
    "UF" -> ''UF ;

    _ -> undefined


data U0 a b c d deriving Typeable
data U1 a b c d deriving Typeable
data U2 a b c d deriving Typeable
data U3 a b c d deriving Typeable
data U4 a b c d deriving Typeable
data U5 a b c d deriving Typeable
data U6 a b c d deriving Typeable
data U7 a b c d deriving Typeable
data U8 a b c d deriving Typeable
data U9 a b c d deriving Typeable
data UA a b c d deriving Typeable
data UB a b c d deriving Typeable
data UC a b c d deriving Typeable
data UD a b c d deriving Typeable
data UE a b c d deriving Typeable
data UF a b c d deriving Typeable

data D0 deriving Typeable ; instance Show D0 where { show _ = "0" } ; instance Name D0
data D1 deriving Typeable ; instance Show D1 where { show _ = "1" } ; instance Name D1
data D2 deriving Typeable ; instance Show D2 where { show _ = "2" } ; instance Name D2
data D3 deriving Typeable ; instance Show D3 where { show _ = "3" } ; instance Name D3
data D4 deriving Typeable ; instance Show D4 where { show _ = "4" } ; instance Name D4
data D5 deriving Typeable ; instance Show D5 where { show _ = "5" } ; instance Name D5
data D6 deriving Typeable ; instance Show D6 where { show _ = "6" } ; instance Name D6
data D7 deriving Typeable ; instance Show D7 where { show _ = "7" } ; instance Name D7
data D8 deriving Typeable ; instance Show D8 where { show _ = "8" } ; instance Name D8
data D9 deriving Typeable ; instance Show D9 where { show _ = "9" } ; instance Name D9

data A_ deriving Typeable ; instance Show A_ where { show _ = "A" } ; instance Name A_
data B_ deriving Typeable ; instance Show B_ where { show _ = "B" } ; instance Name B_
data C_ deriving Typeable ; instance Show C_ where { show _ = "C" } ; instance Name C_
data D_ deriving Typeable ; instance Show D_ where { show _ = "D" } ; instance Name D_
data E_ deriving Typeable ; instance Show E_ where { show _ = "E" } ; instance Name E_
data F_ deriving Typeable ; instance Show F_ where { show _ = "F" } ; instance Name F_
data G_ deriving Typeable ; instance Show G_ where { show _ = "G" } ; instance Name G_
data H_ deriving Typeable ; instance Show H_ where { show _ = "H" } ; instance Name H_
data I_ deriving Typeable ; instance Show I_ where { show _ = "I" } ; instance Name I_
data J_ deriving Typeable ; instance Show J_ where { show _ = "J" } ; instance Name J_
data K_ deriving Typeable ; instance Show K_ where { show _ = "K" } ; instance Name K_
data L_ deriving Typeable ; instance Show L_ where { show _ = "L" } ; instance Name L_
data M_ deriving Typeable ; instance Show M_ where { show _ = "M" } ; instance Name M_
data N_ deriving Typeable ; instance Show N_ where { show _ = "N" } ; instance Name N_
data O_ deriving Typeable ; instance Show O_ where { show _ = "O" } ; instance Name O_
data P_ deriving Typeable ; instance Show P_ where { show _ = "P" } ; instance Name P_
data Q_ deriving Typeable ; instance Show Q_ where { show _ = "Q" } ; instance Name Q_
data R_ deriving Typeable ; instance Show R_ where { show _ = "R" } ; instance Name R_
data S_ deriving Typeable ; instance Show S_ where { show _ = "S" } ; instance Name S_
data T_ deriving Typeable ; instance Show T_ where { show _ = "T" } ; instance Name T_
data U_ deriving Typeable ; instance Show U_ where { show _ = "U" } ; instance Name U_
data V_ deriving Typeable ; instance Show V_ where { show _ = "V" } ; instance Name V_
data W_ deriving Typeable ; instance Show W_ where { show _ = "W" } ; instance Name W_
data X_ deriving Typeable ; instance Show X_ where { show _ = "X" } ; instance Name X_
data Y_ deriving Typeable ; instance Show Y_ where { show _ = "Y" } ; instance Name Y_
data Z_ deriving Typeable ; instance Show Z_ where { show _ = "Z" } ; instance Name Z_

data A  deriving Typeable ; instance Show A  where { show _ = "a" } ; instance Name A
data B  deriving Typeable ; instance Show B  where { show _ = "b" } ; instance Name B
data C  deriving Typeable ; instance Show C  where { show _ = "c" } ; instance Name C
data D  deriving Typeable ; instance Show D  where { show _ = "d" } ; instance Name D
data E  deriving Typeable ; instance Show E  where { show _ = "e" } ; instance Name E
data F  deriving Typeable ; instance Show F  where { show _ = "f" } ; instance Name F
data G  deriving Typeable ; instance Show G  where { show _ = "g" } ; instance Name G
data H  deriving Typeable ; instance Show H  where { show _ = "h" } ; instance Name H
data I  deriving Typeable ; instance Show I  where { show _ = "i" } ; instance Name I
data J  deriving Typeable ; instance Show J  where { show _ = "j" } ; instance Name J
data K  deriving Typeable ; instance Show K  where { show _ = "k" } ; instance Name K
data L  deriving Typeable ; instance Show L  where { show _ = "l" } ; instance Name L
data M  deriving Typeable ; instance Show M  where { show _ = "m" } ; instance Name M
data N  deriving Typeable ; instance Show N  where { show _ = "n" } ; instance Name N
data O  deriving Typeable ; instance Show O  where { show _ = "o" } ; instance Name O
data P  deriving Typeable ; instance Show P  where { show _ = "p" } ; instance Name P
data Q  deriving Typeable ; instance Show Q  where { show _ = "q" } ; instance Name Q
data R  deriving Typeable ; instance Show R  where { show _ = "r" } ; instance Name R
data S  deriving Typeable ; instance Show S  where { show _ = "s" } ; instance Name S
data T  deriving Typeable ; instance Show T  where { show _ = "t" } ; instance Name T
data U  deriving Typeable ; instance Show U  where { show _ = "u" } ; instance Name U
data V  deriving Typeable ; instance Show V  where { show _ = "v" } ; instance Name V
data W  deriving Typeable ; instance Show W  where { show _ = "w" } ; instance Name W
data X  deriving Typeable ; instance Show X  where { show _ = "x" } ; instance Name X
data Y  deriving Typeable ; instance Show Y  where { show _ = "y" } ; instance Name Y
data Z  deriving Typeable ; instance Show Z  where { show _ = "z" } ; instance Name Z


class Hex a where hex :: a -> Int

data X0 deriving Typeable ; instance Hex X0 where hex _ =  0
data X1 deriving Typeable ; instance Hex X1 where hex _ =  1
data X2 deriving Typeable ; instance Hex X2 where hex _ =  2
data X3 deriving Typeable ; instance Hex X3 where hex _ =  3
data X4 deriving Typeable ; instance Hex X4 where hex _ =  4
data X5 deriving Typeable ; instance Hex X5 where hex _ =  5
data X6 deriving Typeable ; instance Hex X6 where hex _ =  6
data X7 deriving Typeable ; instance Hex X7 where hex _ =  7
data X8 deriving Typeable ; instance Hex X8 where hex _ =  8
data X9 deriving Typeable ; instance Hex X9 where hex _ =  9
data XA deriving Typeable ; instance Hex XA where hex _ = 10
data XB deriving Typeable ; instance Hex XB where hex _ = 11
data XC deriving Typeable ; instance Hex XC where hex _ = 12
data XD deriving Typeable ; instance Hex XD where hex _ = 13
data XE deriving Typeable ; instance Hex XE where hex _ = 14
data XF deriving Typeable ; instance Hex XF where hex _ = 15

instance (Hex a, Hex b, Hex c, Hex d) => Show (U0 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U1 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 1 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U2 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 2 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U3 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 3 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U4 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 4 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U5 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 5 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U6 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 6 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U7 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 7 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U8 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 8 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (U9 a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 9 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UA a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 10 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UB a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 11 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UC a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 12 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UD a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 13 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UE a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 14 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Show (UF a b c d) where
    show _ = let xA = hex :: a -> Int ; xB = hex :: b -> Int
                 xC = hex :: c -> Int ; xD = hex :: d -> Int
             in  [toEnum $ 15 * 16^4 + xA __ * 16^3 + xB __ * 16^2 + xC __ * 16 + xD __]

instance (Hex a, Hex b, Hex c, Hex d) => Name (U0 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U1 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U2 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U3 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U4 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U5 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U6 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U7 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U8 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (U9 a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UA a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UB a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UC a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UD a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UE a b c d)
instance (Hex a, Hex b, Hex c, Hex d) => Name (UF a b c d)


