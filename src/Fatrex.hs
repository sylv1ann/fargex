{-|
Module: Fatrex
Description: Finite automaton to regular expression transformation
-}

module Fatrex ( toRegex ) where

import Fadef

type Regex = String

toRegex :: IO ()
toRegex = do
    putStr "Enter a finite state machine: "
    fsmString  <- getLine
    putStrLn ("Your entered FSM: " ++ fsmString)
    let fsm     = read fsmString::FSM
        result  = createRegexFromFSMs $ reduceFSM fsm
    if isCorrectFSM fsm then
        putStrLn ("Regular expression created from given FSM is: " ++ result)
    else error "The FSM is entered incorrectly."
    

getToTrans :: State -> FSM -> [SimpleTrans]
getToTrans q (FSM trans initial final) =
    [(p,a,q) | (p, a, list_p) <- trans, elem q list_p]

-- >>> getToTrans 2 (FSM [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])
-- [(1,"b",2),(3,"b",2)]
--

getFromTrans :: State -> FSM -> [SimpleTrans]
getFromTrans q (FSM trans _ _) =
    [(q,a,r) | (p, a, list_p) <- trans, r <- list_p, p == q]

-- >>> getFromTrans 2 (FSM [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])

{- | For each final state f in the given fsm, all states except f and initial states are
     removed and the transitions between them are taken as regular expressions. The result
     is expressed as list of reduced FSMs.
-}
reduceFSM :: FSM -> [FSM]
reduceFSM fsm@(FSM _ initial final) = map (\f -> removeStates fsm (apart fsm f)) final 

{- | Function, which reduces given FSM by removing all states in the second argument. 
     For each state f all transitions going towards and from this state are taken, and 
     new transitions are created such that the state f is omitted. The result is then 
     reduced further until the no more states are intended to be removed.
-}
removeStates :: FSM -> [State] -> FSM
removeStates fsm [] = fsm -- stav, kedy sú už všetky stavy buď počiatočné alebo koncové
removeStates fsm@(FSM trans initial final) (state:states) = 
    let toTrans   = getToTrans state fsm
        fromTrans = getFromTrans state fsm
        addedTransitions = [(p,x ++ y, [r])|(p,x,q) <- toTrans, (q,y,r) <- fromTrans]
        removedTransitions = removeTransitions (fromTrans ++ toTrans) trans
        newTransitions = removedTransitions ++ addedTransitions
        newFinal = (filter ((/= state)) final)
    in removeStates (FSM newTransitions initial newFinal) states

-- | The wrapper around transition reduction.
removeTransitions :: [SimpleTrans] -> [Transition] -> [Transition]
removeTransitions toBeRemoved trans = foldr (removeTransition) trans toBeRemoved

{- | The transition (State, Regex, State) to be removed from transitions.
     Such a transition has to be found in the list of all transitions.
-}
removeTransition :: SimpleTrans -> [Transition] -> [Transition]
removeTransition _ [] = []
removeTransition (p,x,q) ((r,y,list_r):trans) = 
    if p /= r || x /= y then
        (r,y,list_r):removeTransition (p,x,q) trans
    else if newResult /= [] then
            (r,y,newResult):trans 
         else trans
    where newResult = filter ((/=) q) list_r

{- | The function responsible for removing transitions which begin and also end in same states.
     Since multiple transitions between the same two states can be understood as logical 'or'
     in regular expression, such transitions can be combined into a single transition expressing
     the choice by adding the '+' sign between them.
     Example: [(1,"ab",3),(1,"aa+ba",3)] -> [(1,"(ab)+(aa+ba)",3)]
-}
mergeTransitions :: [SimpleTrans] -> [SimpleTrans]
mergeTransitions trans = connectTransitions trans 0

{- | Executes the merging itself by iterating through the transitions and comparing their starting 
     and final states. If these are equal, then a new list of transitions is created and is merged again.
     If no further reductions are possible for the first transition, then next transition is being
     compared with the rest of the list.
     If there are no transitions left and no reductions are possible, then the result is returned.
-}
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

{- | Function responsible for comparing a transition with the rest of the list.
     If a match is found, then new regex is created as "edge description" and the
     rest of the list is compared again.
-}
connect :: SimpleTrans -> [SimpleTrans] -> [SimpleTrans]
connect t [] = [t]
connect t@(p,x,list_p) ((q,y,list_q):ts) = 
    if p == q && list_p == list_q then
        let result = (p, "(" ++ x ++ ")+(" ++ y ++ ")", list_p)
        in mergeTransitions (result:ts)
    else (q,y,list_q):connect t ts

{- | Wrapper around regex creation. The function takes the list of reduced FSM which contain only
     initial and final states and for each of these FSMs a regex is formed. 
     The result is then created by union ("+") of these regular expressions.
-}
createRegexFromFSMs :: [FSM] -> Regex
createRegexFromFSMs fsms
    | null fsms         = "$"
    | length fsms == 1  = 
        let reducedTransitions = mergeTransitions $ getSimpleTransitions $ head fsms
            initial = getInitial $ head fsms
            final   = getFinal $ head fsms
        in createRegex reducedTransitions initial final
    | otherwise         = foldr (\(FSM trans initial final) acc -> "(" ++ (createRegex (toSimpleTrans trans) initial final) ++ ")+" ++ acc ) "$" fsms

-- | For given list of (unique) transitions the regex is created. All states in transitions are supposed to be either initial or final. 
createRegex :: [SimpleTrans] -> [State] -> [State] -> Regex
createRegex [] _ _ = "$"
createRegex [(_,x,_)] _ _ = x
createRegex trans initial final
    | initial == final =    let listR = [ x | (p,x,q) <- trans, p == q]
                                r = if null listR then "$" else head listR
                            in "(" ++ r ++ ")*"
    | otherwise =   let listR = [ x | (p,x,q) <- trans, elem p initial, p == q]
                        listS = [ x | (p,x,q) <- trans, elem p initial, elem q final]
                        listU = [ x | (p,x,q) <- trans, elem p final, p == q]
                        listT = [ x | (p,x,q) <- trans, elem p final, elem q initial]
                        r = if null listR then "$" else head listR
                        s = if null listS then "$" else head listS
                        u = if null listU then "$" else head listU
                        t = if null listT then "$" else head listT
                    in "(" ++ r ++ "+" ++ s ++ "(" ++ u ++ ")*" ++ t ++ ")*" ++ s ++ "(" ++ u ++ ")*"
    

-- (FSM [(1,"a",[2]),(1,"b",[3,4]),(2,"b",[4]),(3,"a",[4]),(3,"b",[2])] [1] [4])

-- (FSM [(1,"b",[2]),(2,"a",[3]),(3,"a",[1,3]),(3,"b",[2,3])] [1] [3])
