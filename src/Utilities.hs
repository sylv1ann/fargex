{-| 
Module: Utilities
Description: Utilities
-}

module Utilities where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)

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
-- toto mi správne otočí zadaný regex a doplní . tam kde by mal byť konkatenovaný -> takýto regex môžem hodiť do postfixu

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

-- dalsie priklady
-- a(b)
-- a(b)c
-- a(b)c(d)
-- a*(b)
-- ['a'..'z']
-- "a(b)c(d)e(f)g(h)"