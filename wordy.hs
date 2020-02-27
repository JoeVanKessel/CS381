module Wordy where
import Data.Char
--
-- * Syntax of Wordy
--

-- Grammar for Wordy:


--    word       ::= (any string)
--    letter     ::= (any char)
--    num        ::= (any integer)
--    bool       ::= `true`  |  `false`



--    prog       ::= cmd*


--    value     ::= string
--                | int
--                | bool
--                | error

--    cmd        ::= sentence
--                 |  number
--                 |  `count`
--                 |  `reverse`
--                 |  `insert`
--                 |  `remove`
--                 |  `if` cmd `else` cmd `end`
--

-- Program Examples

-- 1. Combine two sentences into one
-- 2. comapre the length of two sentences
-- 3.
-- 4.
-- 5.


data Prog = P [Expr]
  deriving (Eq,Show)

type Var = String

data Expr = Sentence String
         | Num Int
         | Bind Expr Expr
         | Count Expr
         | Reverse Expr
         | Insert Expr Expr Expr
         | Remove Expr Expr
         -- | Capitalize Expr
         | Lowercase Expr
         | Equ Expr Expr
         | IfElse Expr Expr Expr
         -- | Contains Wordy Char
  deriving (Eq,Show)

data Value
   = S String
   | I Int
   | B Bool
   | Error
  deriving (Eq,Show)


listString :: Expr -> [String]
listString (Sentence givenString) = words givenString

countWords :: Expr -> Value
countWords (Sentence sentence) = I (length (listString (Sentence sentence)))


reverseSentence :: Expr -> Value
reverseSentence (Sentence sentence) = S (unwords (reverse (listString (Sentence sentence))))

insertWord :: Expr -> Expr -> Expr -> Value
insertWord (Num pos) (Sentence word) (Sentence sentence) = S (unwords (atPos ++ (word:list)))
                  where (atPos,list) = splitAt pos (listString (Sentence sentence))


--insertWord (Num 2) (Sentence "good") (Sentence "Today is a")
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

cmd :: Expr -> Value
cmd (Sentence x) = S x
cmd (Num x) = I x
--cmd (Bind x y) =
cmd (Count x) = countWords x
cmd (Reverse x) = reverseSentence x
cmd (Insert z y x) = insertWord z y x
cmd (Equ y z)  = case (cmd y, cmd z) of
                   (B a, B b) -> B (a == b)
                   (S i, S j) -> B (i == j)
                   _ -> Error
cmd (IfElse z y x) = case cmd z of
                   B True  -> cmd y
                   B False -> cmd x
                   _ -> Error


-- Syntactic Sugar

true :: Expr
true = Equ (Sentence "x y") (Sentence "x y")

false :: Expr
false = Equ (Sentence "x y") (Sentence "x y z")

and :: Expr -> Expr -> Expr
and l r = IfElse l r false

or :: Expr -> Expr -> Expr
or l r = IfElse l true r





-- Command Examples:

--cmd (IfElse (Equ (Sentence "Hello") (Sentence "Hello")) (Reverse (Sentence "Hello")) (Count (Sentence "Hello")))





-- Wordy Programs:

-- a program to compare the number of words of one sentence to another, if same return True, if not return false

--OLD 
--compareWordCount :: String -> String -> Bool 
--compareWordCount sentence sentence2 = (countWords sentence) == (countWords sentence2) 

--p1 :: Prog
--p1 = P [(Bind (Var "x") (Sentence "Hello World")), (Bind (Var "y") (Sentence "Bye World")), (IfElse (Equ (Count (Var "x") (Count (Var "y")))) 
      --(Insert (Num 0) (Sentence "Hello") (Var "x")) (Capitalize (Var "y")))]


-- a program to insert a period after every word of the sentence

p2 :: Prog
p2 = P [(Insert (Count (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a "))]

-- Same but bad program

p3 :: Prog
p3 = P [(Insert (Reverse (Sentence "Today is a ")) (Sentence "good day") (Sentence "Today is a "))]