{-|
Module: Fsmtrex
Description: Finite automaton to regular expression transformation
-}

module Fsmtrex ( toRegex ) where

import FsmDef
import Data.Char (isAlphaNum)
import System.IO

type Regex = String -- ^ The String in the Transition = (State, String, [State]) may be also understood as a regular expression.

-- | "Main" function of the Fsmtrex module. Takes the input - an FSM from the user and returns created regex.
toRegex :: IO ()
toRegex = do
    putStr "Enter a finite state machine: "
    hFlush stdout
    fsmString  <- getLine
    putStrLn ("Your entered FSM: " ++ fsmString)
    let fsm     = read fsmString :: FSM
        result  = createRegexFromFSMs $ reduceFSM fsm
    if isCorrectFSM fsm then
        putStrLn ("Regular expression created from given FSM is: " ++ result)
    else error "The FSM is entered incorrectly."
    
-- | Returns all transitions from the given FSM which end in the given state 'q'.
getToTrans :: State -> FSM -> [SimpleTrans]
getToTrans q (FSM trans initial final) =
    [(p,a,q) | (p, a, list_p) <- trans, elem q list_p]

-- >>> getToTrans 2 (FSM [(1,'b',[2]),(2,'a',[3]),(3,'a',[1,3]),(3,'b',[2,3])] [1] [3])
-- [(1,"b",2),(3,"b",2)]
--

-- | Returns all transitions from the given FSM which start in the given state 'q'.
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
removeStates fsm [] = fsm           -- all states are already reduced -> only one initial states and one final state is left 
removeStates fsm@(FSM trans initial final) (state:states) = 
    let toTrans   = getToTrans state fsm    
        fromTrans = getFromTrans state fsm  
        addedTransitions = [(p,x ++ y, [r])|(p,x,q) <- toTrans, (q,y,r) <- fromTrans] -- creates new transition which omits the removed state
        removedTransitions = removeTransitions (fromTrans ++ toTrans) trans           -- removes original transitions from the FSM 
        newTransitions = removedTransitions ++ addedTransitions                       -- replaces old transitions by the new ones  
        newFinal = (filter ((/= state)) final)                                        -- updating the set of final states  
    in removeStates (FSM newTransitions initial newFinal) states                      -- removing the rest of the states from the updated FSM  

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
        (r,y,list_r):removeTransition (p,x,q) trans -- when transitions do not match 
    else if newResult /= [] then
            (r,y,newResult):trans                   
         else trans                                 -- if the 'q' was the last ending state for a transition, such a transition is removed completely
    where newResult = filter ((/=) q) list_r        -- removing transition which ends in 'q'

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
    in  if all (\x -> elem x res) (t:ts) then       -- the result can not be reduced anymore for the first transition in the list -> either no more reductions are possible or another transition must be tried to be merged
            if n == length (t:ts) then              
                res                                 -- all possibilities were tried -> no more possible reductions 
            else connectTransitions res (n + 1)     -- trying another transition
        else connectTransitions res n               -- trying further reductions

{- | Function responsible for comparing a transition with the rest of the list.
     If a match is found, then new regex is created as "edge description" and the
     rest of the list is compared again.
-}
connect :: SimpleTrans -> [SimpleTrans] -> [SimpleTrans]
connect t [] = [t]
connect t@(p,x,list_p) ((q,y,list_q):ts) = 
    if p == q && list_p == list_q then              -- two transitions which start and end in the same set of states -> these can be combined into a single one
        let result = (p, "(" ++ x ++ ")+(" ++ y ++ ")", list_p) -- parentheses added to avoid ambiguity
        in mergeTransitions (result:ts)             -- trying to merge again
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

{- | For given list of (unique) transitions the regex is created (based on the Automata and Grammars lecture notes).
     All states in transitions are supposed to be either initial or final. 
-}
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
