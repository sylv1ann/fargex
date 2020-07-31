{- |
Module: Regtfa
Description: Regular expression to finite automaton transformation
-}

module Regtfa ( toFinAuto ) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)

type Regex = String
type Stack = String 
type Input = String

toFinAuto :: IO ()
toFinAuto = do
    input <- getLine
    putStrLn input


toPrefix :: Regex -> Regex
toPrefix regex = reverse $ toPostfix $ replaceChar "()" ")(" $ reverse $ dotComplete regex

replaceChar :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceChar _ _ [] = []
replaceChar old new (x:xs) 
    | indexOf /= -1 = new !! indexOf : replaceChar old new xs
    | otherwise     = x : replaceChar old new xs
    where   indexOf = fromMaybe (-1) $ elemIndex x old

-- >>> replaceChar "()" ")(" $ reverse "((ab+c)+a(bc)*+b)*" 
-- "*(b+*(cb)a+(c+ba))"
--

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

-- >>> dotComplete "(01)*11(01)*(0+1)*00"
-- "(0.1)*1.1.(0.1)*.(0+1)*0.0"
--


-- ['a'..'z']
-- a(b)
-- a(b)c
-- a(b)c(d)
-- "a(b)c(d)e(f)g(h)"
-- a*(b)
-- "((ab+c)+a(bc)*+b)*"
-- ((a*b+c)+a*(b*c)^+b)^

toPostfix :: String -> String
toPostfix expr = toPostfix' [] expr

toPostfix' :: Stack -> Input -> String
toPostfix' s [] = s
toPostfix' [] (x:xs) = if elem x ".(+*" then toPostfix' [x] xs else x : toPostfix' [] xs
toPostfix' stack@(s:_) (x:xs) 
    | x == ')'              =   let (newStack, newExpr) = pop "(" stack False 
                                in newExpr ++ toPostfix' newStack xs
    | x == '(' ||
      x == '*' || 
      (x == '.' && s == '+')= toPostfix' (x:stack) xs
    | isAlphaNum x             = x : toPostfix' stack xs
    | x == '+'              =   let (newStack, newExpr) = pop "(+" stack True 
                                in newExpr ++ toPostfix' newStack xs 
    | x == '.'              =   let (newStack, newExpr) = pop "(." stack True 
                                in newExpr ++ toPostfix' newStack xs 
    | otherwise             = undefined


-- >>> pop "(." "+" True
-- (".","+")
--

-- >>> toPostfix "c+b.a"
-- "cba.+"
--

-- >>> toPostfix "(c+b.a)"
-- "cba.+"
--

-- >>> toPostfix "*(b+*(c.b).a+(c+b.a))"
-- "bcb.*+a.cba.++*"
--

pop :: Input -> Stack -> Bool -> (Stack, String)
pop c [] flag = if not flag then ([], []) else ([c !! 1], []) 
pop c (s:ss) flag = 
    let (stack, expr) = pop c ss flag
    in  if elem s c then 
            if flag
                then (((c !! 1):s:ss), []) 
                else (ss, []) 
        else (stack, s:expr) 

-- >>> replaceChar "()" ")(" $ reverse $ dotComplete "((ab+c)+a(bc)*+b)*"
-- "*(b+*(c.b).a+(c+b.a))"
--
-- toto mi správne otočí zadaný regex a doplní . tam kde by mal byť konkatenovaný -> takýto regex môžem hodiť do postfixu

-- "(01)*11(01)*(0+1)*00"