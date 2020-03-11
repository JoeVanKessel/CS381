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

data Type = TInt | TBool | TString | Error String
    deriving (Eq, Show)

data Expr = Sentence String
         | Num Int
         | Let Var Expr   --Defined Variable (Let "x" (Num 1))
         | Ref Var        --
         | Fun [Expr] Expr
         | App Expr Expr
         | Count Expr
        -- | Split Expr Expr
         | Reverse Expr
         | Insert Expr Expr Expr
        --  | Remove Expr Expr (WIP)
         | Equ Expr Expr
         | IfElse Expr Expr Expr
        --  | While Expr Expr (WIP)
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
   | F [Var] Expr
   | RuntimeError
  deriving (Eq,Show)


-- type Decl = (Var,Type)
-- data Prog = P [Decl] Stmt
--   deriving (Eq,Show)

-- type Env a = Map Var a

listString :: Expr -> [String]
listString (Sentence givenString) = words givenString

countWords :: Expr -> Value
countWords (Sentence sentence) = I (length (listString (Sentence sentence)))


reverseSentence :: Expr -> Value
reverseSentence (Sentence sentence) = S (unwords (reverse (listString (Sentence sentence))))

insertWord :: Expr -> Expr -> Expr -> Value
insertWord (Num pos) (Sentence word) (Sentence sentence) = S (unwords (atPos ++ (word:list)))
                  where (atPos,list) = splitAt pos (listString (Sentence sentence))


-- capitalize :: Expr -> Expr
-- capitalize [] = []
-- capitalize sentence = capWord (single) ++ " " ++ (capitalize (unwords list)) -- remove space at the end?
--                 where (single:list) = listString (Sentence sentence)
-- 
-- allCap:: String -> Expr
-- allCap sentence = map toUpper (Sentence sentence)
-- 
-- allLow:: String -> Expr
-- allLow sentence = map toLower (Sentence sentence)

capWord :: Expr -> Expr
capWord (Sentence []) = Sentence []
capWord (Sentence (x:xs)) = Sentence (toUpper x : map toLower xs)

lowWord :: Expr -> Expr
lowWord (Sentence []) = Sentence []
lowWord (Sentence (x:xs)) = Sentence (toLower x : map toLower xs)

type Env a = [(Var,a)]

cmd :: Expr -> Env Value  ->Value
cmd (Sentence x) _   = S x
cmd (Num x) _        = I x
cmd (Count x)      m = case cmd x of
                          S x' -> countWords (Sentence x')
                          _    -> Error
cmd (Reverse x)    m= case cmd x of
                          S x' -> reverseSentence (Sentence x')
                          _    -> Error
cmd (Insert z y x) m= case (cmd z, cmd y, cmd x) of 
                          (I z', S y', S x') -> insertWord (Num z') (Sentence y') (Sentence x')
                          _                  -> Error
cmd (Equ y z)      m= case (cmd y, cmd z) of
                          (I a, I b) -> B (a == b)
                          (B a, B b) -> B (a == b)
                          (S i, S j) -> B (i == j)
                          _          -> Error
cmd (Fun xs e)     m= F xs e
cmd (App l r)      m= case (cmd l m,cmd r m) of 
                      (F x e,v) -> cmd e ((x,v):m)
cmd (Ref x)        m= fromJust(lookup x m)
cmd (IfElse z y x) m= case cmd z of
                          B True  -> cmd y
                          B False -> cmd x
                          _       -> Error

-- Various semantics

-- type Env a = [(Var,a)]

-- data DVal
--   = DI Int      -- integers
--   | Ds String
--   | DF Var Exp  -- functions
--  deriving (Eq,Show)

-- dsem :: Exp -> Env DVal -> Value
-- dsem (Lit i) = DI i
-- dsem (Sentence s) = DS s
-- dsem (Count e) = case (dsem e) of
--                   DS  

-- Syntactic Sugar

true :: Expr
true = Equ (Sentence "x y") (Sentence "x y")

false :: Expr
false = Equ (Sentence "x y") (Sentence "x y z")

and :: Expr -> Expr -> Expr
and x y = IfElse x y false

or :: Expr -> Expr -> Expr
or x y = IfElse x true y

typeExpr :: Expr -> Env Var -> Type
typeExpr (Num _) _ = TInt
typeExpr (Sentence _) m = TString
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

-- p2 :: Expr
-- p2 = Insert (Count (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a ")

-- -- Same but bad program where Insert is taking the String instead of Num

-- p3 :: Expr
-- p3 = Insert (Reverse (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a ")

-- -- a program that compares two string word counts to see if they are equal

-- p4 :: Expr 
-- p4 = IfElse (Equ (Count (Sentence "Good day John")) (Count (Sentence "Good day John"))) (true) (false)

-- -- Same but bad program, where Equ is comparing String and Num

-- p5 :: Expr 
-- p5 = IfElse (Equ (Count (Sentence "Good day John")) (Reverse (Sentence "Good day John"))) (true) (false)