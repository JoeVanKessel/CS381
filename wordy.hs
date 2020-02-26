module Wordy where
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
type Sentence = [Wordy]
type Prog = [Cmd]

type OneWord = String

data Wordy = Verb String
          | Adj String
          | Noun String
          | Adverb String
          | Pronoun String
          | Prepisition String
          | Conjunction String
          | Interjection String
          | Determiner String
  deriving (Eq,Show)

data Cmd = Count String
         | Reverse String
         | Insert Int OneWord String
         | Compare String String
         | Contains Wordy Char
         | IfElse Prog Prog
  deriving (Eq,Show)


cmd :: Cmd -> String
--cmd (Count x) = countWords x
cmd (Insert x y z) = insertWord x y z
cmd (Reverse x) = reverseSentence x

--cmd (Insert w s i) = insert w s i
--insert :: Wordy -> Sentence -> Int -> Sentence
--insert w s i = let (ys, zs) = splitAt i s in ys ++ [w] ++ zs

-- Insert (Adverb loudly) [(Noun tom), (Verb ran), (Adj fast)] 2

listString :: String -> [String]
listString givenString = words givenString

countWords :: String -> Int
countWords sentence = length (listString sentence)

compareWordCount :: String -> String -> Bool 
compareWordCount sentence sentence2 = (countWords sentence) == (countWords sentence2) 

reverseSentence :: String -> String
reverseSentence sentence = unwords (reverse (listString sentence))

insertWord :: Int -> OneWord -> String -> String 
insertWord pos word wordList = unwords (atPos ++ (word:list))
                  where (atPos,list) = splitAt pos (listString wordList)





--safeDiv :: Float -> Float -> Either String Float
--safeDiv x 0 = Left "Divison by zero"
--safeDiv x y = Right (x / y)
