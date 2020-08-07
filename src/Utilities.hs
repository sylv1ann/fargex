{-| 
Module: Utilities
Description: Miscellaneous functions used in the program.
-}

module Utilities where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)


{-| replaceChar takes two lists of characters of equal length and a string and returns the same string with all occurrences of
    characters in the first list replaced by the characters on the same position in the second list.
    In case the second list is empty, then all characters from the first list are deleted from the given string.
    Simulates command "tr" in bash.
-}
replaceChar :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceChar _ _ [] = []
replaceChar old [] (x:xs) = if elem x old then
                                replaceChar old [] xs
                            else x : replaceChar old [] xs
replaceChar old new (x:xs) 
    | indexOf /= -1 = new !! indexOf : replaceChar old new xs
    | otherwise     = x : replaceChar old new xs
    where   indexOf = fromMaybe (-1) $ elemIndex x old

-- >>> replaceChar "()" ")(" $ reverse "((ab+c)+a(bc)*+b)*" 
-- "*(b+*(cb)a+(c+ba))"
--

-- >>> replaceChar "()" "" $ reverse $ dotComplete "(01)*11(01)*(0+1)*00"
-- "0.0.*(1+0).*(1.0).1.1.*(1.0)"
--

-- >>> replaceChar "()" ")(" $ reverse $ dotComplete "((ab+c)+a(bc)*+b)*"
-- "*(b+*(c.b).a+(c+b.a))"
--

{-| Completes a string with '.' representing the concatenation based on the following rules: 
    - at least two following alphanumeric characters: ab => a.b, ab0d => a.b.0.d
    - two parentheses (with or without * operator between them): (a)(b)c => (a).(b).c, (aa)*(b) => (a.a)*.(b)
    - an alphanumeric character followed by a left parentheses: a(b) => a.(b)
    - star operator followed by an alphanumeric character: a*b => a*.b, 01*ab => 0.1*.a.b
-}
dotComplete :: String -> String
dotComplete []    = []
dotComplete [x] = [x]
dotComplete (x:y:xs)
    | (isAlphaNum x && isAlphaNum y) || (elem x ")*" && elem y (['a'..'z'] ++ ['0' .. '9'] ++ "("))    = x:'.':dotComplete (y:xs)
    | isAlphaNum x && y == '(' = x:'.':y:dotComplete xs
    | otherwise = x:dotComplete (y:xs)

-- >>> dotComplete "((a.b+c)+a.(b.c)*+b)*"
-- "((a.b+c)+a.(b.c)*+b)*"
--

-- Testing examples for dotComplete function:
-- "a(b)" -> expected: a.(b)
-- "a(b)c" -> expected: a.(b).c
-- "a(b)c(d)" -> expected: a.(b).c.(d)
-- "a*(b)" -> expected: a*.(b)
-- "['a'..'z']" -> expected: a.b.c.d.e. ... .x.y.z