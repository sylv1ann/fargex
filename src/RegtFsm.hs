{-|
Module: RegtFsm
Description: Regular expression to finite state machine transformation.
-}

module RegtFsm (toFSM) where

import Utilities
import FsmDef
import Data.Char (isAlphaNum)
import System.IO

type Regex = String -- ^ Synonym for clarification.
type Stack = String -- ^ Synonym for stack representation.
type Input = String -- ^ Synonym for input string.

{-| List of supported operators in a regex:
    '+': logical or,
    '.': concatenation,
    '*': iteration
-}
operator = "+.*"

-- | "Main" function of the Regtfsm module. Takes the input from the user and returns created FSM.
toFSM :: IO ()
toFSM = do
    putStr "Enter a regular expression: "
    hFlush stdout
    regex  <- getLine
    putStrLn ("Your entered regex: " ++ regex)
    let postfixRegex = toPostfix $ dotComplete regex
        result = convertRegexToFSM $ postfixRegex
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
    |  x == ')'                 =   let (newStack, newExpr) = stackChange "" "(" stack False 
                                    in newExpr ++ toPostfix' newStack xs (par - 1)
    |  x == '('                 =   toPostfix' (x:stack) xs (par + 1)
    |  x == '*' || 
      (x == '.' && s == '+')    =   toPostfix' (x:stack) xs par
    |  isAlphaNum x || x == '$' =   x : toPostfix' stack xs par
    |  x == '+'                 =   let (newStack, newExpr) = stackChange "+" "(" stack True 
                                    in newExpr ++ toPostfix' newStack xs par
    |  x == '.'                 =   let (newStack, newExpr) = stackChange "." "+(" stack True 
                                    in newExpr ++ toPostfix' newStack xs par
    |  otherwise                =   error (x : " is unsupported character in regex.")


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
-- expected: ("","")

-- "(01)*11(01)*(0+1)*00" -> "01.*1.1.01.*.01+*.0.0."
-- "((ab+c)+a(bc)*+b)*"   -> "ab.c+abc.*.+b+*"
-- "abba"                 -> "ab.b.a."
-- "$"

-- | Function wrapper responsible for the regex (already in postfix form) evaluation. 
convertRegexToFSM :: Regex -> FSM
convertRegexToFSM regex =   if regex /= "$" then 
                                convert regex [] 0
                            else FSM [] [] []


{-| Regex -> FSM converter. Takes a regex in postfix, list of intermediate results (FSMs)
     and the minimal number if any new state needs to be added.
 -}
convert :: Regex -> [FSM] -> Int -> FSM
convert [] res _ = head res
convert (x:xs) res minN
    | isAlphaNum x || x == '$'  =   let newFSM = (FSM [(minN,[x],[succ minN])] [minN] [succ minN]) -- pridá do zoznamu nový jednoduchý automat (stavA, symbol abecedy) -> stavB, ktorý je aj konečným
                                    in convert xs (newFSM:res) (succ $ succ minN)

    | x == '+'                  =   if length res >= 2 then
                                        let plusResult = plusRegex (res !! 1) (res !! 0)
                                            newRes     = plusResult:(drop 2 res)
                                            newMinN    = succ $ maximum $ getStates plusResult
                                        in  convert xs newRes newMinN
                                    else error "Malformed regex exception."
    | x == '.'                  =   if length res >= 2 then 
                                        let dotResult = dotRegex (res !! 1) (res !! 0)       -- nový automat ktorý vznikne konkatenáciou prvého a druhého prvku res
                                            newRes    = dotResult:(drop 2 res)               -- nový res, ktorý vznikne tak že pôvodné dva automaty v res nahradí jedným novým spojeným 
                                            newMinN   = succ $ maximum $ getStates dotResult -- nová hodnota minimálneho čísla stavu <- vznikne ako najvyššia hodnota starého stavu + 1
                                        in convert xs newRes newMinN
                                    else error "Malformed regex exception."
    | x == '*'                  =   if length res >= 1 then
                                        let starResult = starRegex $ head res
                                            newRes     = starResult:(drop 1 res)
                                            newMinN    = succ $ maximum $ getStates starResult
                                        in convert xs newRes newMinN
                                    else error "Malformed regex exception."
    | otherwise                 =   error "Unsupported regex error."

-- | Creates new FSM from two given FSMs by adding new initial and final state and the possibility to continue to either of the original FSMs.
plusRegex :: FSM -> FSM -> FSM
plusRegex r@(FSM r_trans r_initial r_final) s@(FSM s_trans s_initial s_final) =
    FSM (initial_lambda ++ r_trans ++ s_trans ++ final_lambda) (new_initial) (new_final)
    where stateNumMax  = last $ (getStates r) ++ (getStates s)   -- last is the maximum -> state numbers are sorted ascending <-> interchangeable for "maximum"
          new_initial    = [stateNumMax + 1]                     -- new initial state set  
          new_final    = [stateNumMax + 2]
          initial_lambda = [(head new_initial, "$", r_initial ++ s_initial)]
          final_lambda = [(x,"$", new_final) | x <- r_final ++ s_final] 
                
-- | Creates new FSM from two given FSMs by concatenating them.
dotRegex :: FSM -> FSM -> FSM
dotRegex (FSM r_trans r_initial r_final) (FSM s_trans s_initial s_final) =
    FSM (r_trans ++ lambda_R_S ++ s_trans) r_initial s_final
    where lambda_R_S = [(x,"$", [y]) | x <- r_final, y <- s_initial]

-- | Creates new FSM from a given FSM by adding the iteration possibility.
starRegex :: FSM -> FSM
starRegex fa@(FSM trans initial final) =
    FSM (initial_lambda ++ trans ++ final_lambda) new_initial new_final
    where stateNumMax    = last $ getStates fa
          new_initial    = [stateNumMax + 1]
          new_final      = [stateNumMax + 2]
          initial_lambda = [(head new_initial, "$", initial ++ new_final)]
          final_lambda   = [(x, "$", initial ++ new_final) | x <- final]   

-- >>> dotRegex (FSM [(0,'a',[1])] [0] [1]) (FSM [(2,'b',[3])] [2] [3])
-- FSM [(0,'a',[1]),(1,'$',[2]),(2,'b',[3])] [0] [3]
--

-- >>> plusRegex (FSM [(0,'a',[1])] [0] [1]) (FSM [(2,'b',[3])] [2] [3])
-- FSM [(4,'$',[0,2]),(0,'a',[1]),(2,'b',[3]),(1,'$',[5]),(3,'$',[5])] [4] [5]
--

-- >>> starRegex (FSM [(0,'a',[1])] [0] [1])
-- FSM [(2,'$',[0,3]),(0,'a',[1]),(1,'$',[0,3])] [2] [3]
--
