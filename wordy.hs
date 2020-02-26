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

type Sentence = [Word]

type Prog = [Cmd]

data Word = Verb String
          | Adj String
          | Noun String
          | Adverb String
          | Pronoun Sting
          | Prepisition String
          | Conjunction String
          | Interjection String
          | Determiner String
  deriving (Eq,Show)

data Cmd = Count Word
         | Reverse Sentence
         | Contains Word Char
         | IfElse Prog Prog
  deriving (Eq,Show)
