module Wordy where


import Data.Maybe (fromJust)
import Data.Char
-- import Data.Map (Map,fromList,lookup,insert)

-- import Prelude hiding (lookup)
--
-- * Syntax of Wordy
--

-- Grammar for Wordy:

--    prog       ::= cmd*


--    value     ::= string
--                | int
--                | bool
--                | error

--    cmd        ::= sentence
--                 |  number
--                 |  `bind` expr
--                 |  `count` expr
--                 |  `reverse` expr
--                 |  `insert` expr
--                 |  `remove` expr
--                 |  `if` expr expr `else` expr
--



type Prog = [Expr]
type Var = String

data Type = TInt | TBool | TString | TFun | Error String
    deriving (Eq, Show)

data Expr = Sentence String
         | Num Int
         | Count Expr
         | Split Expr
         | Cap Expr
         | Low Expr
         | Reverse Expr
         | Insert Expr Expr Expr
         | Remove Expr Expr
         | Let Var Expr Expr
         | Ref Var
         | Fun Var Expr
         | App Expr Expr
         | Equ Expr Expr
         | IfElse Expr Expr Expr
  deriving (Eq,Show)

-- data Stmt = Bind Var Expr
--           | IfElse Expr Stmt Stmt
--           | While Expr Stmt
--           | Block [Stmt]
--   deriving (Eq,Show)

data Value
   = S String
   | L [String]
   | I Int
   | B Bool
   | F Var Expr
   | RuntimeError
   | ErrorVal String
  deriving (Eq,Show)


-- type Decl = (Var,Type)
-- data Prog = P [Decl] Stmt
--   deriving (Eq,Show)

-- type Env a = Map Var a


-- A helper function to split a string into a list of strings
listString :: Expr -> [String]
listString (Sentence givenString) = words givenString

-- Imp of the split in the expr level
split :: Expr -> Value
split (Sentence givenString) = L (words givenString)

-- Function to count the words in a given string
countWords :: Expr -> Value
countWords (Sentence sentence) = I (length (listString (Sentence sentence)))

-- Function to reverse a given string
reverseSentence :: Expr -> Value
reverseSentence (Sentence sentence) = S (unwords (reverse (listString (Sentence sentence))))

-- Function to insert a string into another string at a given position.
insertWord :: Expr -> Expr -> Expr -> Value
insertWord (Num pos) (Sentence word) (Sentence sentence) = S (unwords (atPos ++ (word:list)))
                  where (atPos,list) = splitAt pos (listString (Sentence sentence))

-- Function to remove a word from a specified location
removeWord :: Expr -> Expr -> Value
removeWord (Num pos) (Sentence sentence) = S (unwords (_removeWord pos (listString (Sentence sentence))))

_removeWord :: Int -> [a] -> [a]
_removeWord 0 (x:xs) = xs
_removeWord num (x:xs) | num >= 0 = x : _removeWord (num - 1) xs

-- Function to capitilize the first letter of the string
capWord :: Expr -> Value
capWord (Sentence []) = S []
capWord (Sentence (x:xs)) = S (toUpper x : map toLower xs)

-- Function to lowercase the first letter of every string
lowWord :: Expr -> Value
lowWord (Sentence []) = S []
lowWord (Sentence (x:xs)) = S (toLower x : map toLower xs)

type Env a = [(Var,a)]


-- Command function which implements all of the core level functions in our language
cmd :: Expr -> Env Value -> Value
cmd (Sentence x) _   = S x
cmd (Num x) _        = I x
cmd (Count x)      m = case cmd x m of
                          S x' -> countWords (Sentence x')
                          _    -> RuntimeError
cmd (Reverse x)    m= case cmd x m of
                          S x' -> reverseSentence (Sentence x')
                          _    -> RuntimeError
cmd (Insert z y x) m= case (cmd z m, cmd y m, cmd x m) of 
                          (I z', S y', S x') -> insertWord (Num z') (Sentence y') (Sentence x')
                          _                  -> RuntimeError
cmd (Remove x y)    m= case (cmd x m, cmd y m) of
                          (I x', S y') -> removeWord (Num x') (Sentence y')     
                          _ -> RuntimeError
cmd (Equ y z)      m= case (cmd y m, cmd z m) of
                          (I a, I b) -> B (a == b)
                          (B a, B b) -> B (a == b)
                          (S i, S j) -> B (i == j)
                          _          -> RuntimeError
cmd (Let x b e) m = case cmd b m of
                    v -> cmd e ((x,v):m)
cmd (Fun x e)     _ = F x e
cmd (App l r)      m = case (cmd l m,cmd r m) of 
                      (F x e,v) -> cmd e ((x,v):m)
                      _ -> RuntimeError
cmd (Ref x)        m= case lookup x m of
                      Nothing -> RuntimeError
                      _       -> fromJust(lookup x m ) 

cmd (IfElse z y x) m= case cmd z m of
                          B True  -> cmd y m
                          B False -> cmd x m
                          _       -> RuntimeError
cmd (Split x)      m= case cmd x m of 
                          S x' -> split (Sentence x')
                          _       -> RuntimeError
cmd (Cap x)        m= case cmd x m of 
                          S x' -> capWord (Sentence x')
                          _       -> RuntimeError
cmd (Low x)        m= case cmd x m of 
                          S x' -> lowWord (Sentence x')
                          _       -> RuntimeError


--capitalize :: Expr
--capitalize (Sentence x) = Cap (Sentence (Split (Sentence x)))

-- Syntactic Sugar

true :: Expr
true = Equ (Sentence "x y") (Sentence "x y")

false :: Expr
false = Equ (Sentence "x y") (Sentence "x y z")

and :: Expr -> Expr -> Expr
and x y = IfElse x y false

or :: Expr -> Expr -> Expr
or x y = IfElse x true y

ctv :: Expr -> Value
ctv (Sentence s) = S s
ctv (Num a) = I a

cte :: Var -> Expr -> Env Value -> Env Value 
cte v e ev = [(v,ctv(e))]++ev 

typeExpr :: Expr -> Env Value -> Type
typeExpr (Num _) _ = TInt
typeExpr (Sentence _) _ = TString
typeExpr (Count x) m = case typeExpr x m of 
                        TString -> TString
                        _       -> Error "Type Error"
typeExpr (Reverse x) m = case typeExpr x m of 
                        TString -> TString
                        _       -> Error "Type Error"
typeExpr (Insert i s1 s2) m = case (typeExpr i m, typeExpr s1 m, typeExpr s2 m) of
                              (TInt, TString, TString) -> TString
                              _                        -> Error "Type Error"
typeExpr (Equ x y) m = case (typeExpr x m, typeExpr y m) of
                        (TInt, TInt)       -> TBool
                        (TBool, TBool)     -> TBool
                        (TString, TString) -> TBool
                        _                  -> Error "Type Error"
typeExpr (Let r e1 e2) m = case (typeExpr e1 (cte r e1 m), typeExpr e2 (cte r e1 m)) of
                          (_, Error a)  -> Error a
                          (Error a, _) -> Error a
                          (TInt, _) -> TInt
                          (TString, _) -> TString
                          (TFun, _)    -> TFun
                          (TBool, _)   -> TBool
typeExpr (Ref x) m = case lookup x m of 
                          Just (S a) -> typeExpr (Sentence a) m
                          Just (I a) -> typeExpr (Num a) m
                          _ -> Error  "Undefined Variable"
typeExpr (Cap x)        m = case typeExpr x m of 
                            TString -> TString
                            _ -> Error "Type Error"
typeExpr (Split x)      m = case typeExpr x m of
                        TString -> TString
                        _                  -> Error "Type Error"
typeExpr (Low x)      m = case typeExpr x m of 
                        TString -> TString
                        _                  -> Error "Type Error"

typeExpr (IfElse x _ _) m = case typeExpr x m of
                              TBool -> TBool
                              _     -> Error "Type Error"
typeExpr (App _ _) _ = TString
typeExpr (Remove p s) m = case (typeExpr p m, typeExpr s m) of
                        (TInt, TString)-> TString
                        _       -> Error "Type Error"
typeExpr (Fun _ _)             _ = TString


-- typeProg :: Prog -> Type
-- typeProg [] = ty


--------------------------
-- Command Examples:

--cmd (IfElse (Equ (Sentence "Hello") (Sentence "Hello")) (Reverse (Sentence "Hello")) (Count (Sentence "Hello")))

-- Wordy Programs:

-- a program that is still under development with the later implimentation of Bind
--p1 :: Prog
--p1 = P [(Bind (Var "x") (Sentence "Hello World")), (Bind (Var "y") (Sentence "Bye World")), (IfElse (Equ (Count (Var "x") (Count (Var "y")))) 
      --(Insert (Num 0) (Sentence "Hello") (Var "x")) (Capitalize (Var "y")))]

----------------------------


-----------------Working Programs-------------


-- -- a program to insert a period after every word of the sentence

p2 :: Expr
p2 = Insert (Count (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a ")

-- -- Same but bad program where Insert is taking the String instead of Num

-- p3 :: Expr
-- p3 = Insert (Reverse (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a ")

-- -- a program that compares two string word counts to see if they are equal

p4 :: Expr 
p4 = IfElse (Equ (Count (Sentence "Good day John")) (Count (Sentence "Good day John"))) (true) (false)

-- -- Same but bad program, where Equ is comparing String and Num

-- p5 :: Expr 
-- p5 = IfElse (Equ (Count (Sentence "Good day John")) (Reverse (Sentence "Good day John"))) (true) (false)

p6 :: Expr
p6 = Let "str" (Sentence "Hello") $ Let "f" (Fun "x" (Insert (Count (Ref "str") ) (Ref "x") (Ref "str"))) $ App (Ref "fs") (Sentence "world")

p7 :: Expr
p7 = Let "B" (Num 233) (Ref "B")

-- p8 :: Expr
-- p8 = Let "i" (Num 10) $ Let "f" (Fun "x" (IfElse (Equ (Ref "i") (Num 1))()(Ref "i")))

theRun :: Expr -> Value
theRun e = case typeExpr e [] of
           Error a -> ErrorVal a
           _       -> cmd e []
 
