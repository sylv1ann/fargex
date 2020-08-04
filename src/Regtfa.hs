{-|
Module: Regtfa
Description: Module responsible for the conversion of given regular expression to the FSM.
-}

module Regtfa {-(toFinAuto)-} where

import Utilities
import Fadef
import Data.Char (isAlphaNum)

-- | Data type synonyms for better argument understanding. 
type Regex = String
type Stack = String 
type Input = String

{-| List of supported operators in a regex:
    '+': logical or,
    '.': concatenation,
    '*': iteration
-}
operator = "+.*"

-- | "Main" function of the Regtfa module. Takes the input from the user, processes it and returns created FSM.
toFinAuto :: IO ()
toFinAuto = do
    putStr "Enter a regular expression: "
    regex  <- getLine
    putStrLn ("Your entered regex: " ++ regex)
    let postfixRegex = toPostfix $ dotComplete regex
        result = convertRegexToFA $ postfixRegex
    putStrLn ("Your regex in postfix notation: " ++ postfixRegex)
    putStrLn $ show result
    
-- | Given a Regex in the form of a String, this is transformed to the prefix form.
toPrefix :: Regex -> Regex
toPrefix regex = 
    reverse $ toPostfix $ replaceChar "()" ")(" $ reverse $ dotComplete regex

-- | Given a Regex in the form of a String, this is transformed to the postfix form.
toPostfix :: Regex -> Regex
toPostfix expr = toPostfix' [] expr 0

-- | Helper function which evaluates given expression and returns the regex in its postfix form.
toPostfix' :: Stack -> Input -> Int -> Regex
toPostfix' stack []    par  =   if par == 0  then  
                                    stack
                                else error "Unmatched parentheses -> RegEx is incorrect"
toPostfix' []  (x:xs)  par
    | x == '('  =   toPostfix' [x] xs (par + 1)
    | x == ')'  =   error "Unmatched parentheses -> RegEx is incorrect." -- pre prípad napr.: "abb(a"
    | otherwise =   if elem x operator then
                        toPostfix' [x] xs par
                    else x : toPostfix' [] xs par
toPostfix' stack@(s:_) (x:xs) par
    |  x == ')'             =   let (newStack, newExpr) = stackChange "" "(" stack False 
                                in newExpr ++ toPostfix' newStack xs (par - 1)
    |  x == '('             =   toPostfix' (x:stack) xs (par + 1)
    |  x == '*' || 
      (x == '.' && s == '+')=   toPostfix' (x:stack) xs par
    |  isAlphaNum x         =   x : toPostfix' stack xs par
    |  x == '+'             =   let (newStack, newExpr) = stackChange "+" "(" stack True 
                                in newExpr ++ toPostfix' newStack xs par
    |  x == '.'             =   let (newStack, newExpr) = stackChange "." "+(" stack True 
                                in newExpr ++ toPostfix' newStack xs par
    |  otherwise            =   error (x : " is unsupported character in regex.")


-- >>> toPostfix "c+b.a"
-- "cba.+"
--

-- >>> toPostfix "(c+b.a)"
-- "cba.+"
--

-- >>> toPostfix "*(b+*(c.b).a+(c+b.a))"
-- "bcb.*+a.cba.++*"
--


{- | Performs changes on the stack during the transformation to postfix form. 
     Returns a pair of String: 
        - first element represents new, updated content of the stack,
        - second element represents the list of all popped elements from the stack. 
-}
stackChange :: Input -> String -> Stack -> Bool -> (Stack, String)
stackChange ch c [] flag =  if not flag then
                                ([], []) 
                            else ([head ch], []) 
stackChange ch c (s:ss) flag = 
    let (stack, expr) = stackChange ch c ss flag
    in  if elem s c then 
            if flag then 
                (((head ch):s:ss), []) 
            else (ss, []) 
        else (stack, s:expr) 


-- stackChange "" "(" "" False
-- expected result: ("","")

-- "(01)*11(01)*(0+1)*00" -> "01.*1.1.01.*.01+*.0.0."
-- "((ab+c)+a(bc)*+b)*"   -> "ab.c+abc.*.+b+*"
-- "abba"                 -> "ab.b.a."
--'ϵ'

-- | Function wrapper responsible for the regex (already in postfix form) evaluation. 
convertRegexToFA :: Regex -> FA
convertRegexToFA regex = convert regex [] 0


{-| Regex -> FA converter. Takes a regex in postfix, list of intermediate results (FSMs)
     and the minimal number if any new state needs to be added.
 -}
convert :: Regex -> [FA] -> Int -> FA
convert [] res _ = head res
convert (x:xs) res minN
    | isAlphaNum x  =   let newFA = (FA [(minN,[x],[succ minN])] [minN] [succ minN]) -- pridá do zoznamu nový jednoduchý automat (stavA, symbol abecedy) -> stavB, ktorý je aj konečným
                        in convert xs (newFA:res) (succ $ succ minN)

    | x == '+'      =   if length res >= 2 then
                            let plusResult = plusRegex (res !! 1) (res !! 0)
                                newRes     = plusResult:(drop 2 res)
                                newMinN    = succ $ maximum $ getStates plusResult
                            in  convert xs newRes newMinN
                        else error "Malformed regex exception."
    | x == '.'      =   if length res >= 2 then 
                            let dotResult = dotRegex (res !! 1) (res !! 0)       -- nový automat ktorý vznikne konkatenáciou prvého a druhého prvku res
                                newRes    = dotResult:(drop 2 res)               -- nový res, ktorý vznikne tak že pôvodné dva automaty v res nahradí jedným novým spojeným 
                                newMinN   = succ $ maximum $ getStates dotResult -- nová hodnota minimálneho čísla stavu <- vznikne ako najvyššia hodnota starého stavu + 1
                            in convert xs newRes newMinN
                        else error "Malformed regex exception."
    | x == '*'      =   if length res >= 1 then
                            let starResult = starRegex $ head res
                                newRes     = starResult:(drop 1 res)
                                newMinN    = succ $ maximum $ getStates starResult
                            in convert xs newRes newMinN
                        else error "Malformed regex exception."
    | otherwise     =   error "Unsupported regex error."

-- | Creates new FSM from two given FSMs by adding new initial and final state and the possibility to continue to either of the original FSMs.
plusRegex :: FA -> FA -> FA
plusRegex r@(FA r_trans r_initial r_final) s@(FA s_trans s_initial s_final) =
    FA (initial_lambda ++ r_trans ++ s_trans ++ final_lambda) (new_initial) (new_final)
    where stateNumMax  = last $ (getStates r) ++ (getStates s)   -- last is the maximum -> state numbers are sorted ascending <-> interchangeable for "maximum"
          new_initial    = [stateNumMax + 1]                     -- new initial state set  
          new_final    = [stateNumMax + 2]
          initial_lambda = [(head new_initial, "$", r_initial ++ s_initial)]
          final_lambda = [(x,"$", new_final) | x <- r_final ++ s_final] 
                
-- | Creates new FSM from two given FSMs by concatenating them.
dotRegex :: FA -> FA -> FA
dotRegex (FA r_trans r_initial r_final) (FA s_trans s_initial s_final) =
    FA (r_trans ++ lambda_R_S ++ s_trans) r_initial s_final
    where lambda_R_S = [(x,"$", [y]) | x <- r_final, y <- s_initial]

-- | Creates new FSM from a given FSM by adding the iteration possibility.
starRegex :: FA -> FA
starRegex fa@(FA trans initial final) =
    FA (initial_lambda ++ trans ++ final_lambda) new_initial new_final
    where stateNumMax    = last $ getStates fa
          new_initial    = [stateNumMax + 1]
          new_final      = [stateNumMax + 2]
          initial_lambda = [(head new_initial, "$", initial ++ new_final)]
          final_lambda   = [(x, "$", initial ++ new_final) | x <- final]   

-- >>> dotRegex (FA [(0,'a',[1])] [0] [1]) (FA [(2,'b',[3])] [2] [3])
-- FA [(0,'a',[1]),(1,'$',[2]),(2,'b',[3])] [0] [3]
--

-- >>> plusRegex (FA [(0,'a',[1])] [0] [1]) (FA [(2,'b',[3])] [2] [3])
-- FA [(4,'$',[0,2]),(0,'a',[1]),(2,'b',[3]),(1,'$',[5]),(3,'$',[5])] [4] [5]
--

-- >>> starRegex (FA [(0,'a',[1])] [0] [1])
-- FA [(2,'$',[0,3]),(0,'a',[1]),(1,'$',[0,3])] [2] [3]
--

-- >>> toFinAuto c+d* --> testuje aj správne poradie operácií pri vyhodnocovaní regexu
-- s parametrami pri + a . 0 1: FA [(2,'$',[4,0]),(4,'$',[2,5]),(2,'d',[3]),(3,'$',[2,5]),(0,'c',[1]),(5,'$',[3]),(1,'$',[3])] [2] [3] -- nesprávne
-- s parametrami pri + a . 1 0: FA [(6,'$',[0,4]),(0,'c',[1]),(4,'$',[2,5]),(2,'d',[3]),(3,'$',[2,5]),(1,'$',[7]),(5,'$',[7])] [6] [7] -- správne

-- >>> toFinAuto c+ba
-- FA [(6,'$',[0,2]),(0,'c',[1]),(2,'b',[3]),(3,'$',[4]),(4,'a',[5]),(1,'$',[7]),(5,'$',[7])] [6] [7] -- správne