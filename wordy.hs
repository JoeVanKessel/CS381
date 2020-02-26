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



type Word = String
type Sentence = [Word]

type Prog = [Cmd]


data Cmd = PushN Int
         | PushB Bool
         | Count word
         | Reverse Sentence
         | Contains word Char
         | IfElse Prog Prog
  deriving (Eq,Show)
