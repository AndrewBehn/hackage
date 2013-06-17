{- LambdaToolbox - Version and Copyright information below -}

import System.IO
import System.Info

license = "LambdaToolbox v1.0.1"
    ++ "\n"
    ++ "    by Julian Fleischer <julian dot fleischer at fu-berlin dot de>\n"
    ++ "\n"
    ++ "    A tiny toolbox for working with lambda expressions.\n"
    ++ "    Includes alpha and beta reduction, reducing to normal form,\n"
    ++ "    identifying free variables, recognizing and expanding macros.\n"
    ++ "\n"
    ++ "    Redistribution and use in source and binary forms,\n"
    ++ "    with or without modification, are permitted provided that\n"
    ++ "    the following conditions are met:\n"
    ++ "\n"
    ++ "    1. Redistributions of source code must retain the above\n"
    ++ "       copyright notice, this list of conditions and the\n"
    ++ "       following disclaimer.\n"
    ++ "\n"
    ++ "    2. Redistributions in binary form must reproduce the above\n"
    ++ "       copyright notice, this list of conditions and the following\n"
    ++ "       disclaimer in the documentation and/or other materials\n"
    ++ "       provided with the distribution.\n"
    ++ "\n"
    ++ "    THIS SOFTWARE IS PROVIDED \"AS IS\" AND ANY EXPRESS OR IMPLIED\n"
    ++ "    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n"
    ++ "    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n"
    ++ "    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR\n"
    ++ "    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n"
    ++ "    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT\n"
    ++ "    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"
    ++ "    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n"
    ++ "    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"
    ++ "    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR\n"
    ++ "    OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,\n"
    ++ "    EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"

data Lambda = Func String Lambda | Expr [Lambda] | Name String

isLambda = (`elem` "λ/^\\")
isSpace  = (`elem` " \t\n")

-- Lambdas are compared by structural equality
instance Eq Lambda where
    l1 == l2 = equals [] (norm l1) [] (norm l2)
        where
            equals b1 (Name x1) b2 (Name x2) = indexOf x1 b1 == indexOf x2 b2
            equals b1 (Func f1 x1) b2 (Func f2 x2) = equals (f1:b1) x1 (f2:b2) x2
            equals b1 (Expr x1) b2 (Expr x2)
                | length x1 == length x2 = all id (map (\(l,r) -> equals b1 l b2 r) (zip x1 x2))
            equals _ _ _ _ = False

-- Pretty printing lambda expressions
instance Show Lambda where
    show (Name a) = a
    show func@(Func a x) = 'λ' : name ++ "." ++ show rest
        where
            (name, rest) = showFunc a x
            showFunc a (Func a' x) = showFunc (a ++ a') x
            showFunc a rest = (a, rest)
    show (Expr [])   = ""
    show (Expr expr) = showExpr expr
        where
            showExpr [] = ""
            showExpr (x@(Func _ _):xs) = "(" ++ show x ++ ")" ++ showExpr xs
            showExpr (x@(Expr _):xs)   = "(" ++ show x ++ ")" ++ showExpr xs
            showExpr (x@(Name _):xs)   = show x ++ showExpr xs

-- Parsing lambda strings (accepts literally *anything*)
instance Read Lambda where
    readsPrec x = wrap . norm . fst . readExpr []
        where
            wrap x = [(x, "")]

            readExpr :: [Lambda] -> String -> (Lambda, String)
            readExpr soFar "" = (Expr $ reverse soFar, "")
            readExpr soFar (x:xs)
                | isSpace x  = readExpr soFar xs
                | x == '('   =
                    let (expr, rest) = readExpr [] xs
                    in readExpr (expr : soFar) rest
                | x == ')'   = (Expr $ reverse soFar, xs)
                | isLambda x =
                    let (func, rest) = readFunc xs
                    in readExpr (func : soFar) (')' : rest)
--                | isLambda x = 
--                    let (func, rest) = readFunc xs
--                    in readExpr (func : soFar) rest
--                | x == '.'   =
--                    error "Dot not allowed here"
                | otherwise  = readExpr (Name [x] : soFar) xs

            readFunc [] = (Name "?", "")
            readFunc (x:xs)
                | isSpace x  = readFunc xs
--                | isLambda x =
--                    error "Lambda not allowed here"
--                | x == '(' || x == ')' =
--                    error "Parenthesis not allowed here"
                | x == '.' =
                    let (expr, rest) = readExpr [] xs
                    in (expr, rest)
                | otherwise =
                    let (expr, rest) = readFunc xs
                    in (Func [x] expr, rest)

-- “normalize” a lambda expression (without doing reductions)
norm :: Lambda -> Lambda
norm name@(Name _) = name
norm (Func var def) = Func var $ norm def
norm (Expr [x]) = norm x
norm (Expr x) = Expr $ map norm x

-- list bound variables (names of abstractions)
bound :: Lambda -> [String]
bound expr = bound' [] expr
    where
        bound' bound (Name _) = bound
        bound' bound (Func var def) = bound' (var:bound) def
        bound' bound (Expr expr) = concat (bound : map (bound' []) expr)

-- list free variables
free :: Lambda -> [String]
free expr = free' [] expr
    where
        free' bound (Name x) = if x `elem` bound then [] else [x]
        free' bound (Func var def) =
            if var `elem` bound
                then free' bound def
                else free' (var:bound) def
        free' bound (Expr expr) = concat $ map (free' bound) expr

allfree :: Lambda -> [(Lambda, [String])]
allfree func@(Func _ def) = if freeVars /= []
        then (func, freeVars) : allfree def
        else allfree def
    where freeVars = free func
allfree (Expr expr) = concat $ map allfree expr
allfree (Name _) = []

-- expand macros
expand :: [(String, Lambda)] -> Lambda -> Lambda
expand macros expr@(Name name) =
    case lookup name macros of
        Just lambda -> expand macros lambda
        Nothing     -> expr
expand macros (Func var def) = Func var $ expand macros def
expand macros (Expr expr) = Expr $ map (expand macros) expr

-- subst macros
subst :: [(String, Lambda)] -> Lambda -> Lambda
subst macros expr = subst' expr
    where
        macros' = map (\(a,b) -> (expand macros b, a)) macros
        subst' expr = 
            case lookup expr macros' of
                Just name -> Name name
                Nothing   -> case expr of
                    Name _       -> expr
                    Func var def -> Func var $ subst' def
                    Expr x       -> Expr $ map subst' x

-- Alpha conversion

alpha :: Lambda -> Lambda
alpha expr = alpha2 (bound expr) expr

alpha2 forbidden expr
    | any (`elem` boundVars) $ freeVars = foldr (\v e -> rename v (newName v) e) expr' freeVars
    | otherwise = expr'
        where
            expr'       = alpha' forbidden expr
            boundVars   = bound expr'
            freeVars    = free expr'
            newName var = findNewName var (forbidden ++ freeVars ++ boundVars)

alpha' forbidden func@(Func var def)
    | var `elem` forbidden = alpha' forbidden $ rename var newName func
    | otherwise            = Func var $ alpha2 (var:forbidden) def
        where
            newName = findNewName var forbidden
alpha' forbidden (Expr expr)    = Expr $ map (alpha2 forbidden) expr
alpha' _ x              = x

rename :: String -> String -> Lambda -> Lambda
rename a a' name@(Name _) = name
rename a a' (Expr expr) = Expr $ map (rename a a') expr
rename a a' func@(Func var def)
    | var == a  = Func a' $ replaceFree a (Name a') def
    | otherwise = Func var $ rename a a' def

findNewName :: String -> [String] -> String
findNewName old forbidden
    | alt == [] = [new | n <- [1..], new <- [old ++ show n], not (elem new forbidden)] !! 0
    | otherwise = alt !! 0
        where
            alt
                | length old == 1 = newLetter (old !! 0)
                | otherwise       = newLetter 'a'
            newLetter start = let source = [start..'z'] ++ ['a'..start]
                in [ [a] | a <- source, not (elem [a] forbidden)]

-- Beta reduction

data BetaResult = Reduced | Impossible | NeedsAlphaConversion
    deriving (Show, Eq)

beta :: Lambda -> (BetaResult, Lambda)
beta = beta'

-- finds an application in the expression or returns (Impossible, ...)
-- if a possible application is found but can not be reduced
-- without renaming, (NeedsAlphaConversion, ...) is returned.
beta' :: Lambda -> (BetaResult, Lambda)
beta' (Name n) = (Impossible, Name n)
beta' (Func var def) = (result, Func var $ def')
    where (result, def') = beta' def
beta' expr@(Expr (Func var def : arg : rest)) = 
    if any (`elem` bound def) $ free arg
        then (NeedsAlphaConversion, expr)
        else (Reduced, norm $ Expr $ replaceFree var arg def : rest)
beta' (Expr expr) = (result, Expr expr')
    where (result, expr') = betaExpr [] expr

-- beta' for an Expr, i.e. a list of Lambdas
betaExpr :: [Lambda] -> [Lambda] -> (BetaResult, [Lambda])
betaExpr soFar [] = (Impossible, reverse soFar)
betaExpr soFar (x:xs) = let (result, expr) = beta x in
    if result == Impossible
        then betaExpr (x:soFar) xs
        else (result, reverse (expr:soFar) ++ xs)

-- replaces all free variables with a given label (first argument)
-- with the given lambda (second argument) in the third argument.
-- returns the reduced lambda expression.
replaceFree :: String -> Lambda -> Lambda -> Lambda
replaceFree var arg name@(Name n)
    | var == n  = arg
    | otherwise = name
replaceFree var arg func@(Func var' def)
    | var == var' = func
    | otherwise   = Func var' $ replaceFree var arg def
replaceFree var arg (Expr expr) = Expr $ map (replaceFree var arg) expr

-- Eta reduction

eta :: Lambda -> (Bool, Lambda)
eta func@(Func v1 (Expr (def : (Name v2) : [])))
    | v1 == v2 && not (v1 `elem` free def) = (True, def)
eta x = (False, x)

data Reduction = Alpha | Beta | Eta | None
    deriving (Show, Eq)

-- trace a complete reduction
trace :: Lambda -> [(Reduction, Lambda)]
trace expr = case result of
                Reduced              -> (Beta, expr')   : trace expr'
                NeedsAlphaConversion -> (Alpha, alpha') : trace alpha'
                Impossible           -> (None, expr)    : []
    where
        alpha'          = alpha $ norm expr
        (result, expr') = beta  $ norm expr

-- Debug a Lambda Expression

debug :: Lambda -> String
debug (Name n@(_:_:_)) = '[' : n ++ "n"
debug (Name n) = n
debug (Func a x) = '(' : 'λ' : a ++ "." ++ debug x ++ ")"
debug (Expr x) = '{' : foldl (++) "" (map debug x) ++ "}"

-- utility functions

lastIndexOf :: Eq a => a -> [a] -> Int
lastIndexOf x xs = lastIndexOf' (-1) 0 x xs
    where
        lastIndexOf' i _ _ [] = i
        lastIndexOf' i c x' (x:xs)
            | x == x'   = lastIndexOf' c (c+1) x' xs
            | otherwise = lastIndexOf' i (c+1) x' xs

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = indexOf' 0 x xs
    where
        indexOf' _ _ [] = (-1)
        indexOf' c x' (x:xs)
            | x == x'   = c
            | otherwise = indexOf' (c+1) x' xs

--firstDup :: Eq a => [a] -> a
--firstDup (x:xs)
--    | x `elem` xs = x
--    | otherwise   = firstDup xs

number :: Lambda -> Maybe Int
number (Expr [x]) = number x
number (Func v1 (Func v2 def)) = number' v1 v2 0 def
    where
        number' v1 v2 c (Name v)
            | v2 == v = Just c
        number' v1 v2 c (Expr ((Name v) : x : []))
            | v1 == v = number' v1 v2 (c+1) x
        number' _ _ _ _ = Nothing
number x = Nothing

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- examples
macros' = [
    ("0", "λsz.z", "Zero."),

    ("1", "λsz.s(z)", "One. Like S0."),
    ("2", "λsz.s(s(z))", "Two. Like S(S0)), or S1"),
    ("3", "λsz.s(s(s(z)))", "Three. Like S(S(S0), or S2."),
    ("4", "λsz.s(s(s(s(z))))", "Four. Like S3."),
    ("5", "λsz.s(s(s(s(s(z)))))", "Five. Like S4."),
    ("6", "λsz.s(s(s(s(s(s(z))))))", "Six. Like S5."),
    ("7", "λsz.s(s(s(s(s(s(s(z)))))))", "Seven. Like S6."),
    ("8", "λsz.s(s(s(s(s(s(s(s(z))))))))", "Eight. Like S7. Or P9."),
    ("9", "λsz.s(s(s(s(s(s(s(s(s(z)))))))))", "Nine. Like S9."),

    ("I", "λx.x", "The identity function."),
    ("S", "λwyx.y(wyx)", "The successor function."),

    ("+", "λab.aSb", "Addition."),
    ("*", "λxyz.x(yz)", "Mulitplication."),
    ("-", "λab.bPa", "Subtraction"),

    ("T", "λxy.x", "True."),
    ("F", "λxy.y", "False."),

    ("∧", "λxy.xyF", "Logical AND."),
    ("∨", "λxy.xTy", "Logical OR."),
    ("¬", "λx.xFT", "Logical NOT."),
    ("H", "λpz.z(S(pT))(pT)", "The Phi function (needed by the predecessor function)."),
    ("P", "λn.nH(λz.z00)F", "The predecessor function."),

    (",", "λab.(λt.tab)", "Creates a tupel from the two values a and b."),

    ("Z", "λx.xF¬F", "Test for Zero."),
    ("G", "λxy.Z(xPy)", "Tests whether the first argument is greater or equal to the second argument."),
    ("E", "λxy.∧(Z(xPy))(Z(yPx))", "Tests whether both arguments constitute the same numeric value."),

    ("Y", "λy.(λx.y(xx))(λx.y(xx))", "The all famous Y combinator (needed for recursion)."),

    ("Q", "λrn.Zn0(nS(r(Pn)))", "Adds up the first n natural numbers. Use with Y, like YR3."),
    ("X", "(λy.(λx.xy))x", "EAT THIS, alpha conversion.")
    ]

macros :: [(String, Lambda)]
macros = [(name, read expr) | (name, expr, _) <- macros']

dupl = [(a, b) | (a, x) <- macros, (b, y) <- macros, a < b && x == y]

macro :: String -> Lambda
macro which = read ([y | (x, y, _) <- macros', x == which] !! 0)

main = do
    putStrLn "Welcome to lambda calculus! Type \"help\" for help."
    repl macros Nothing

showMacros = unlines [n ++ " ≡ " ++ m ++ "\n  " ++ d | (n, m, d) <- macros']

showMacroList [] = "No macros defined."
showMacroList m  = "\n" ++ unlines [n ++ " ≡ " ++ show m | (n, m) <- m]

showHelp = "\n"
        ++ " Type something. Anything not a command\n"
        ++ " will be interpreted as λ-expression.\n"
        ++ "\n"
        ++ " Type “help <command>” for further details, like “help macros”.\n"
        ++ "\n"
        ++ " The commands are:\n"
        ++ "   a  alpha    Performs alpha conversion on the current λ-expression.\n"
        ++ "   b  beta     Performs beta reduction on the current λ-expression.\n"
        ++ "   e  eta      Performs eta reduction on the current λ-expression.\n"
        ++ "   .  trace    Trace complete reduction to normal form.\n"
        ++ "   ,  eval     Expand macros and trace, also only show substitued λ-expr.\n"
        ++ "   x  expand   Expands macros in the current λ-expression.\n"
        ++ "   s  subst    Identifies and substitutes known macros.\n"
        ++ "   f  free     Show free variables in the current λ-expression.\n"
        ++ "   ff allfree  Show *all* free variables, including sub-expressions.\n"
        ++ "   r  readable Shows a readable representation of the current expression.\n"
        ++ "   d  debug    Shows an accurate representation of the internal store.\n"
        ++ "   m  macros   Shows a list of all built-in macros.\n"
        ++ "   cc clear    Clears the list of macros.\n"
        ++ "   h  help     Show this help text.\n"
        ++ "   l  license  Show copyright / version information.\n"
        ++ "   q  exit     Exits the interpreter.\n"
        ++ "\n"
        ++ " let X = <macro>\n"
        ++ "   defines the macro with the name X.\n"
        ++ "\n"
        ++ " λ-functions can be declared using λ,\n"
        ++ " forward- or backward slash (/, \\),\n"
        ++ " or ^, like so: λx.x, /x.x, \\x.x, ^x.x\n"

aliasHelp short long = " “" ++ short ++ "” is the short alias for “"
                    ++ long ++ "”:\n" ++ detailedHelp long

detailedHelp "m"      = aliasHelp "m" "macros"
detailedHelp "macros" = showMacros ++ "\n"
                     ++ " This is a list of built-in macros. The list of current macros\n"
                     ++ " can be obtained using “macros”.\n"
                     ++ " You can define new macros using “let X = ...”\n"
detailedHelp "e"      = aliasHelp "e" "eta"
detailedHelp "eta"    = " Performs a top-level eta reduction on the current expression.\n"
                     ++ " Eta reduction is only applied on top level expressions.\n"
detailedHelp "b"      = aliasHelp "b" "beta"
detailedHelp "beta"   = " Performs beta reduction on the current expression, if possible.\n"
                     ++ " Reduction is performed in normal order, that is, the left-most\n"
                     ++ " expression is evaluated first.\n"
detailedHelp "a"      = aliasHelp "a" "alpha"
detailedHelp "alpha"  = " Performs alpha reduction on the current string.\n"
detailedHelp "."      = aliasHelp "." "solve"
detailedHelp "solve"  = " Solves the current expression by applying beta reduction until\n"
                     ++ " no further reduction is possible. If necessary, alpha conversion\n"
                     ++ " is applied. This might result in a non-terminating computation!\n"
detailedHelp "x"      = aliasHelp "x" "expand"
detailedHelp "expand" = " Expands the macros in the current expression.\n"
                     ++ " For example, this will expand all T in “pt” into\n"
                     ++ " their definition: pT -> p(λxy.x)\n"

detailedHelp cmd      = " No detailed help available for the command “" ++ cmd ++ "”.\n"

showTrace :: (Lambda -> String) -> [(Reduction, Lambda)] -> [String]
showTrace s ((Alpha, expr) : xs) = ("-α-> " ++ s expr) : showTrace s xs
showTrace s ((Beta,  expr) : xs) = ("-β-> " ++ s expr) : showTrace s xs
showTrace s ((Eta,   expr) : xs) = ("-η-> " ++ s expr) : showTrace s xs
showTrace s ((_, expr)     : _ )
    | num /= Nothing = [" ≡   " ++ (show $ fromJust num)]
    | otherwise      = [" ≡   " ++ (s    $ subst macros expr)]
        where
            num :: Maybe Int
            num = number expr
            fromJust (Just x) = x

data Action = Message String | NewState Lambda | NewMacros [(String, Lambda)] | Exit

basicAction macros ('l':'e':'t':' ':name:' ':'=':' ':def)
    | not $ isSpace name = Just $ NewMacros (([name], read def) : macros)
basicAction macros cmd
    | "l" >< "license" = Just $ Message license
    | "h" >< "help"   = Just $ Message $ showHelp
    | "m" >< "macros" = Just $ Message $ showMacroList macros
    | "cc" >< "clear" = Just $ NewMacros []
    | take 5 cmd == "help " = Just $ Message ('\n' : (detailedHelp $ drop 5 cmd))
    | take 2 cmd == "h "    = Just $ Message ('\n' : (detailedHelp $ drop 2 cmd))
    | cmd `elem` ["q", "quit", "exit", ":q"] = Just Exit
    | otherwise       = Nothing
        where a >< b = cmd == a || cmd == b

adv = [ "a", "alpha", "b", "beta", "e", "eta", "x", "expand", "s", "subst",
        "f", "free", "ff", "allfree", "d", "debug", ".", "trace", ",", "eval",
        "r", "readable", "" ]

isWindows = take 5 os == "mingw"

sanitize x
    | isWindows = maskUnicode x
    | otherwise = x
        where
            maskUnicode ('λ':xs) = '\\' : maskUnicode xs
            maskUnicode ('“':xs) = '"'  : maskUnicode xs
            maskUnicode ('”':xs) = '"'  : maskUnicode xs
            maskUnicode ('≡':xs) = '='  : maskUnicode xs
            maskUnicode ('α':xs) = 'a'  : maskUnicode xs
            maskUnicode ('β':xs) = 'b'  : maskUnicode xs
            maskUnicode ('η':xs) = 'n'  : maskUnicode xs
            maskUnicode ('∧':xs) = '^'  : maskUnicode xs
            maskUnicode ('∨':xs) = 'v'  : maskUnicode xs
            maskUnicode (x:xs)   = x    : maskUnicode xs
            maskUnicode []       = []

repl :: [(String, Lambda)] -> Maybe Lambda -> IO ()
repl macros state = do
    line <- getLine
    hSetEncoding stdout (if isWindows then latin1 else utf8)
    let putStrLn x = hPutStrLn stdout $ sanitize x
    let res = case basicAction macros line of
                Just action -> action
                Nothing -> case state of
                    Nothing -> if line `elem` adv
                        then Message  "No lambda expression loaded!"
                        else NewState $ read line
                    Just l  -> eval macros line l
    case res of
        Message message -> putStrLn message       >> repl macros state
        NewState state' -> putStrLn (show state') >> repl macros (Just state')
        NewMacros []    -> putStrLn "Macros cleared." >> repl [] state
        NewMacros m'    -> putStrLn "Macros changed." >> repl m' state
        Exit            -> putStrLn "Goodbye."

eval :: [(String, Lambda)] -> String -> Lambda -> Action
eval _      ""  expr  = Message $ show expr
eval macros cmd expr
    | "a" >< "alpha"  = NewState $ alpha expr
    | "b" >< "beta"   = let (result, expr') = beta expr in
        case result of
            Reduced              -> NewState expr'
            Impossible           -> Message "No further reduction possible!"
            NeedsAlphaConversion -> Message "Needs alpha conversion first!"
    | "e" >< "eta"    = let (result, expr') = eta expr in
        if result then NewState expr' else Message "No top-level eta reduction applicable!"
    | "." >< "trace"  = Message  $ unlines (("     " ++ show expr) : (showTrace show $ trace expr))
    | "," >< "eval"   = let show' = show . subst macros in
            Message  $ unlines (("     " ++ show' expr) : (showTrace show' $ trace $ expand macros expr))
    | "x" >< "expand" = NewState $ expand macros expr
    | "s" >< "subst"  = NewState $ subst macros expr
    | "f" >< "free"   = let freeVars = free expr in
        if freeVars == []
            then Message "No free variables!"
            else Message $ "Free variables: " ++ formatFree freeVars
    | "ff" >< "allfree" = let freeVars = allfree expr in
        if freeVars == []
            then Message "No free variables!"
            else Message $ '\n' : (unlines $ map (\(l, f) -> formatFree f ++ " in " ++ show l) freeVars)
    | "r" >< "readable" = Message $ show $ subst macros $ expr
    | "d" >< "debug"  = Message  $ debug expr
    | otherwise       = NewState $ read cmd
        where a >< b = cmd == a || cmd == b
              formatFree free = foldl1 (\l r -> l ++ ", " ++ r) free

