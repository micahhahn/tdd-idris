module Chapter2

import Prelude.List

-- Exercise 1
t1 : (String, String, String)
t1 = ("A", "B", "C")

t2 : List String
t2 = ["A", "B", "C"]

t3 : ((Char, String), Char)
t3 = (('A', "B"), 'C')

-- Exercise 2
palindrome : String -> Bool
palindrome s = reverse s == s

-- Excercise 3
palindromeI : String -> Bool
palindromeI s = reverse (toLower s) == (toLower s) 

-- Excercise 4
palindrome10 : String -> Bool
palindrome10 s = length s > 10 && reverse s == s

-- Exercise 5
palindromeN : Nat -> String -> Bool
palindromeN n s = length s > n && reverse s == s

