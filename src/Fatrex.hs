{-|
Module: Fatrex
Description: Finite automaton to regular expression transformation
-}

module Fatrex ( toRegex ) where

import Fadef

toRegex :: IO ()
toRegex = undefined

type Regex = String

getToTrans :: State -> FA -> [SimpleTrans]
getToTrans q (FA trans initial final) =
    [(p,a,q) | (p, a, list_p) <- trans, elem q list_p]

-- >>> getToTrans 2 (FA [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])
-- [(1,"b",2),(3,"b",2)]
--

getFromTrans :: State -> FA -> [SimpleTrans]
getFromTrans q (FA trans _ _) =
    [(q,a,r) | (p, a, list_p) <- trans, r <- list_p, p == q]

-- >>> getFromTrans 2 (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])

removeAllStates :: FA -> [FA]
removeAllStates fsm@(FA _ initial final) = map (\f -> removeStates fsm (apart fsm f)) final 

removeStates :: FA -> [State] -> FA
removeStates fsm [] = fsm -- stav, kedy sú už všetky stavy buď počiatočné alebo koncové
removeStates fsm@(FA trans initial final) (state:states) = 
    let toTrans   = getToTrans state fsm
        fromTrans = getFromTrans state fsm
        addedTransitions = [(p,x ++ y, [r])|(p,x,q) <- toTrans, (q,y,r) <- fromTrans]
        removedTransitions = removeTransitions (fromTrans ++ toTrans) trans
        newTransitions = removedTransitions ++ addedTransitions
        newFinal = (filter ((/= state)) final)
    in removeStates (FA newTransitions initial newFinal) states

removeTransitions :: [SimpleTrans] -> [Transition] -> [Transition]
removeTransitions toBeRemoved trans = foldr (removeTransition) trans toBeRemoved

removeTransition :: SimpleTrans -> [Transition] -> [Transition]
removeTransition _ [] = []
removeTransition (p,x,q) ((r,y,list_r):trans) = 
    if p /= r || x /= y then
        (r,y,list_r):removeTransition (p,x,q) trans
    else if newResult /= [] then
            (r,y,newResult):trans 
         else trans
    where newResult = filter ((/=) q) list_r

mergeTransitions :: [SimpleTrans] -> [SimpleTrans]
mergeTransitions trans = connectTransitions trans 0

connectTransitions :: [SimpleTrans] -> Int -> [SimpleTrans]
connectTransitions [] _  = []
connectTransitions [x] _ = [x]
connectTransitions (t:ts) n =
    let res = connect t ts
    in  if all (\x -> elem x res) (t:ts) then
            if n == length (t:ts) then
                res
            else connectTransitions res (n + 1)
        else connectTransitions res n

connect :: SimpleTrans -> [SimpleTrans] -> [SimpleTrans]
connect t [] = [t]
connect t@(p,x,list_p) ((q,y,list_q):ts) = 
    if p == q && list_p == list_q then
        let result = (p, x ++ "+" ++ y, list_p)
        in mergeTransitions (result:ts)
    else (q,y,list_q):connect t ts

createRegexFromFSMs :: [FA] -> Regex
createRegexFromFSMs fsms
    | null fsms         = []
    | length fsms == 1  = 
        let reducedTransitions = getComplexTransitions $ mergeTransitions $ getSimpleTransitions $ head fsms
            initial = getInitial $ head fsms
            final   = getFinal $ head fsms
        in createRegex $ FA reducedTransitions initial final
    | otherwise         = foldr (\fsm acc -> "(" ++ (createRegex fsm) ++ ")+" ++ acc ) [] fsms -- tu to dorob tiež rovnako ako vyššie

createRegex :: FA -> Regex
createRegex (FA [] _ _) = "$"
createRegex (FA [(_,x,_)] _ _) = x
createRegex (FA trans initial final)
    | initial == final =    let reducedTrans = mergeTransitions $ toSimpleTrans trans
                                listR = [ x | (p,x,q) <- reducedTrans, p == q]
                                r = if null listR then "$" else head listR
                            in "(" ++ r ++ ")*"
    | otherwise =   let reducedTrans = mergeTransitions $ toSimpleTrans trans -- decomposition of complex transition to list of simple transitions (3,"a",[3,4]) -> [(3,"a",[3]),(3,"a",[4])]
                        listR = [ x | (p,x,q) <- reducedTrans, elem p initial, p == q]
                        listS = [ x | (p,x,q) <- reducedTrans, elem p initial, elem q final]
                        listU = [ x | (p,x,q) <- reducedTrans, elem p final, p == q]
                        listT = [ x | (p,x,q) <- reducedTrans, elem p final, elem q initial]
                        r = if null listR then "$" else head listR
                        s = if null listS then "$" else head listS
                        u = if null listU then "$" else head listU
                        t = if null listT then "$" else head listT
                    in "(" ++ r ++ "+" ++ s ++ "(" ++ u ++ ")*" ++ t ++ ")*" ++ s ++ "(" ++ u ++ ")*"
    
-- [(a, r ++ "+" ++ s, xs)|((a,r,xs),(b,s,ys)) <- (makePairs list), a == b, xs == ys]

-- (FA [(1,"a",[2]),(1,"b",[3,4]),(2,"b",[4]),(3,"a",[4]),(3,"b",[2])] [1] [4])

-- (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])
-- getToTrans 2 (FA [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])

-- [(1,"b",[4]),(1,"ab",[4]),(1,"ba",[4]),(1,"bbb",[4])]