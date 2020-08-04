{-|
Module: Fatrex
Description: Finite automaton to regular expression transformation
-}

module Fatrex ( toRegex ) where

import Fadef

toRegex :: IO ()
toRegex = undefined

type Regex = String
type Transition = (State, String, [State])

getToTrans :: State -> FA -> [(State, Regex, State)]
getToTrans q fsm@(FA trans initial final) = [(p,[a],q) | (p, [a], list_p) <- trans, elem q list_p]

-- >>> getToTrans 2 (FA [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])
-- [(1,"b",2),(3,"b",2)]
--

getFromTrans :: State -> FA -> [(State, Regex, State)]
getFromTrans q fsm@(FA trans initial final) =
    [(q,[a],r) | (p, [a], list_p) <- trans, r <- list_p, p == q]

-- >>> getFromTrans 2 (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])

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

removeAllStates :: FA -> FA
removeAllStates fsm = removeStates (getNonFinalStates fsm) fsm

removeStates :: [State] -> FA -> FA
removeStates [] fsm = undefined -- stav, kedy sú už všetky stavy buď počiatočné alebo koncové
removeStates (state:states) fsm@(FA trans initial final) = 
    let toTrans   = getToTrans state fsm
        fromTrans = getFromTrans state fsm
        addedTransitions = [(p,x ++ y, [r])|(p,x,q) <- toTrans, (q,y,r) <- fromTrans]
        removedTransitions = removeTransitions (fromTrans ++ toTrans) trans
        newTransitions = removedTransitions ++ addedTransitions
    in (FA newTransitions initial final)


removeTransitions :: [(State, String, State)] -> [Transition] -> [Transition]
removeTransitions toBeRemoved trans = removeTransitions' toBeRemoved trans []

removeTransitions' :: [(State, String, State)] -> [Transition] -> [Transition] -> [Transition]
removeTransitions' [] _ result = result
removeTransitions' _ [] _ = []
removeTransitions' (x:xs) trans acc = 
    let interResult = removeTransition x trans
        newAcc = foldr (\x acc -> setSet x acc) acc interResult
    in removeTransitions' xs trans newAcc
-- concat $ map (\x -> removeTransition x trans) triplets

removeTransition :: (State, String, State) -> [(State, String, [State])] -> [(State, String, [State])]
removeTransition _ [] = []
removeTransition (p,x,q) ((r,y,list_r):trans) = 
    if p /= r || x /= y then
        (r,y,list_r):removeTransition (p,x,q) trans
    else if newResult /= [] then
            (r,y,newResult):trans 
         else trans
    where newResult = filter (\x -> x /= q) list_r


fromTrans = getFromTrans 2 (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])
toTrans   = getToTrans 2 (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])
trans :: [(State, String, [State])]
trans = [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])]
