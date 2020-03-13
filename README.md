# Wordy Language

Wordy - Sentence manipulation language with functional paradigm, that performs various manipulations and edits to strings. This language will take in a command with a given string and return the edited string or the output manipulation and edit.


Team memmbers:  

Yuchen Wen - wenyu  
Yevgeniy Lebid - lebidy  
Joe Van Kessel - vankessj  
Ying Li - liyi4  


A language wich will have the features to:

1. Reverse the words inside of a given sentence.
2. Insert a word into a given sentence at a specified location.
3. Remove a word from a given sentenve at a specified location.
5. Count the words in the sentence.
6. Split sentence into list of words.
7. Compare sentences.
8. Create functions with custom variables.

To run the execute examples programs:

In GHCi:

- :l wordy.hs
- run p1
- run p2
- run p3
- ...

There are 9 examples:

Featured examples: p8 and p9

Good examples: p1, p2, p4, p5, p6, p8, p9

    Will return the intended result as described in code file above each example. 

BAD examples: p3, p7

    Will give Error before execution.


Outputs:


p1 - Will compare strings, if they are same, will reverse if same, if noresult word count. Result S "there Hello"

p2 - Using library feature, will combine two strings together. Maybe used by someone, who is intrested to make one strings out of two strings. Result S "Today is a good day"

p3 - Bad program, where static type catches that the input is incorrect. Result ErrorVal "Type Error"

p4 - Using our library feature, will compare two strings, will return true if equal, false if not equal. Useful programs for comparing large text strings where its hard to know. Result B True

p5 - Program where assigning custom variables can allow to insert into a sentence. Useful when the programmer wants to use variables in diffrent places, so use the same value in multiple locations. Result S "Hello world"

p6 - A simple program that demonstrates the ability to assign variables to an operation and then call them. Result I 233

p7 - Bad program where our static type is checking for valid input. Result ErrorVal "Undefined Variable"

* p8 - A recursive example of a program that removes words from a string until only the specified amount is left. This is a good example of string manipulation if the programmer wants to leave only a specific number of words in a string. Result L ["LAST","TWO"]

* p9 - An example of a program where given capitalized string and/or lowercased string will perform the Cap and Low features of our language. Useful feature for performing string editing. Result S "lowercase Capital lowercase Capital lowercase"


