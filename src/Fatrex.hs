{-|
Module: Fatrex
Description: Finite automaton to regular expression transformation
-}

module Fatrex ( toRegex ) where

import Fadef

toRegex :: IO ()
toRegex = undefined

type Regex = String

getToTrans :: State -> FA -> [(State, Regex, State)]
getToTrans q fsm@(FA trans initial final) = [(p,[a],q) | (p, a, list_p) <- trans, elem q list_p]

-- >>> getToTrans 2 (FA [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])
-- [(1,"b",2),(3,"b",2)]
--

getFromTrans :: State -> FA -> [(State, Regex, State)]
getFromTrans q fsm@(FA trans initial final) =
    [(q,[a],r) | (p, a, list_p) <- trans, r <- list_p, p == q]

-- >>> getFromTrans 2 (FA [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])


{-
- nezabudni dodefinovať pravidlá ak už sú len dva stavy -> jeden initial, jeden final
- asi bude treba zmeniť prechodovú funkciu z Char na String kvôli podpore regexov

foreach x in getNonFinalStates:
    toTrans   = getToTrans x fsm -- [(a,b,x)]
    fromTrans = getFromTrans x fsm -- [(x,e,f)]
    foreach tT in toTrans:
        foreach fT in fromTrans:
            vytvor novú transition z toTrans ktorá ide do toTrans a po znaku ktorý vznikne ako concat tých dvoch transitions

-}