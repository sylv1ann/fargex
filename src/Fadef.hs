{-
Module: Fadef
Description: description
-}

module Fadef where

import Data.List (sort)

type State = Int
type Final = [State]
type Start = [State]
type Alphabet = Char

data FA = FA [(State, Char, [State])] Start Final
    deriving Show

getStates :: FA -> [State]
getStates (FA [] _ _) = []
getStates (FA ((state, _, states):transitions) start final) = 
    sort $ [ y | y <- tryComplete]
    where other = getStates $ FA transitions start final
          tryComplete  = (foldr (\x acc -> setSet x acc) other (state:states)) -- tryComplete skúša doplniť množinu ktorú dostal z other o prvky v (state:states) -> ak uz v mnozine su tak ich neprida


setSet :: (Eq a) => a -> [a] -> [a]
setSet s [] = [s]
setSet s set =  if elem s set then
                    set
                else s:set

isFinal :: State -> FA -> Bool
isFinal state auto@(FA _  _ final) = elem state (getStates auto) && elem state final


-- >>> getStates (FA [(0, 'a', [1]), (1, 'b', [2]), (2, 'b', [3]), (3, 'a', [2,4])])
-- [0,1,2,3,4]
--

-- >>> getStates (FA [(0, 'a', [3,1])])
-- [0,1,3]
--

-- >>> getStates (FA [(0, 'a', [5,1]), (1, 'b', [2,0]), (2, 'c', [1,0,2,3,4])])
-- [0,1,2,3,4,5]
--
