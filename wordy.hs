module Wordy where
import Data.Char
--
-- * Syntax of Wordy
--

-- Grammar for Wordy:
--
--    word       ::= (any string)
--    letter     ::= (any char)
--    num        ::= (any integer)
--    bool       ::= `true`  |  `false`
--    prog       ::= cmd*
--    cmd        ::= word
--                 |  bool
--                 |  `count`
--                 |  `reverse`
--                 |  `inWord`
--                 |  `if` prog `else` prog `end`
--


-- Program Examples

-- 1. Reverse a sentence
-- 2. Insert into a sentence
-- 3. Add/Remove from sentence
-- 4. Create a valid sentence out of words
-- 5. Check if two words are the same


-- 1. Encode the above grammar as a set of Haskell data types


--type Domain = Either Int String
--type SentenceList = [Wordy]
data Prog = P [Expr]
  deriving (Eq,Show)

type OneWord = String

{-- data Wordy = Verb String
          | Adj String
          | Noun String
          | Adverb String
          | Pronoun String
          | Prepisition String
          | Conjunction String
          | Interjection String
          | Determiner String
  deriving (Eq,Show)
--}

data Value
   = S String
   | I Int
   | B Bool
   | Error
  deriving (Eq,Show)

data Expr = Sentence String
         | Num Int
         | Count Expr --expr
         | Reverse Expr
         | Insert Int Expr Expr
         | Remove Expr Expr
         | Capitalize Expr
         | Lowercase Expr

         -- | Compare String Sentence
         -- | Contains Wordy Char
         -- | IfElse Prog Prog
  deriving (Eq,Show)



data Stmt = Bind String Expr
          | If Expr Stmt Stmt
          -- | While Expr Stmt
          -- | Block [Stmt]
  deriving (Eq,Show)

{--
stmt :: Stmt -> Expr
stmt (Bind x y) = x
            where x = y
--}



--cmd :: Expr -> Expr
--cmd (Count x) = countWords x
--cmd (Sen x sen) = let x = sen
--cmd (Insert x y z) = insertWord x y z
--cmd (Reverse x) = reverseSentence x

--cmd (Insert w s i) = insert w s i
--insert :: Wordy -> Sentence -> Int -> Sentence
--insert w s i = let (ys, zs) = splitAt i s in ys ++ [w] ++ zs

-- Insert (Adverb loudly) [(Noun tom), (Verb ran), (Adj fast)] 2

listString :: Expr -> [String]
listString (Sentence givenString) = words givenString

countWords :: Expr -> Int
countWords (Sentence sentence) = length (listString (Sentence sentence))


reverseSentence :: Expr -> Expr
reverseSentence (Sentence sentence) = Sentence (unwords (reverse (listString (Sentence sentence))))

-- insertWord :: Expr -> Expr 
-- insertWord (Insert pos word (Sentence sentence)) = unwords (atPos ++ (word:list))
--                   where (atPos,list) = splitAt pos (listString wordList)

-- capitalize :: String -> Expr
-- capitalize [] = []
-- capitalize sentence = capWord (single) ++ " " ++ (capitalize (unwords list)) -- remove space at the end?
                -- where (single:list) = listString (Sentence sentence)
-- 
-- allCap:: String -> Expr
-- allCap sentence = map toUpper (Sentence sentence)
-- 
-- allLow:: String -> Expr
-- allLow sentence = map toLower (Sentence sentence)
-- 
-- 
-- capWord :: String -> Expr
-- capWord [] = []
-- capWord (x:xs) = toUpper x : map toLower xs
-- 
-- 

{---------------
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (If c st se) m = if evalBool c m
                          then evalStmt st m
                          else evalStmt se m

-- Helper function to evaluate a list of statements. We could also
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)
----------------}

-- Wordy Programs 

-- a program to compare the number of words of one sentence to another, if same return True, if not return false

--compareWordCount :: String -> String -> Bool 
--compareWordCount sentence sentence2 = (countWords sentence) == (countWords sentence2) 

--p1 :: Prog
--p1 = P []




-- a program to insert a period after every word of the sentence
















--safeDiv :: Float -> Float -> Either String Float
--safeDiv x 0 = Left "Divison by zero"
--safeDiv x y = Right (x / y)
