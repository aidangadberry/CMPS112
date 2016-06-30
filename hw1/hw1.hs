-- Assignment 1 - 1/17/16 - Aidan Gadberry agadberr

import Data.Char

citeAuthor :: String -> String -> String
citeAuthor st1 st2 = st2 ++ ", " ++ st1

initials :: String -> String -> String
initials st1 st2 = [st1 !! 0] ++ "." ++ [st2 !! 0] ++ "."

title :: (String, String, Int) -> String
title (st1, st2, num) = st2

citeBook :: (String, String, Int) -> String
citeBook (st1, st2, num) = st2 ++ " (" ++ st1 ++ ", " ++ show (num) ++ ")"

bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec (tup:tups) = citeBook tup ++ "\n" ++ bibliography_rec tups

year :: (String, String, Int) -> Int
year (s1, s2, num) = num
-- take in a tuple (author, title, year), and return the year

averageYear :: [(String, String, Int)] -> Int
averageYear tups = sum (map year tups) `div` length tups

txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2] this is a whale..."

isRef :: String -> Bool
isRef str = head str == '[' && last str == ']' && isDigit (str !! 1)
-- checks to see if the given string follows the format [n]

references :: String -> Int
references text = length (filter isRef (words text))

refNum :: [Char] -> Int
refNum str = digitToInt (str !! 1)
-- returns the number within the reference of format [n]

replaceRef :: [(String, String, Int)] -> String -> String
replaceRef books word = if isRef word
						then citeBook (books !! (refNum (word) - 1))
						else word
-- if the given word isRef, then it will return the book citation of that
-- reference. Otherwise, it will return the original word

refText :: [(String, String, Int)] -> [String] -> [String]
refText books [] = []
refText books (word:left) = [replaceRef books word] ++ refText books left
-- the recursive part of the citeText function. It cycles through each word 
-- in the text, replacing references where needed

citeText :: [(String, String, Int)] -> String -> String
citeText books text = unwords (refText books (words text))
-- takes in the String of text, converts it into a list of Strings, passes it
-- into refText, and finally recombines the resultant list of Strings into an
-- individual String