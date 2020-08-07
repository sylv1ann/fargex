{-|
Module: Fadef
Description: Definition of the data type representing a finite state machine (FSM).
-}

module Fadef where

import Data.List (sort)

type State = Int
type Final = [State]
type Start = [State]
type Alphabet = Char
type Transition  = (State, String, [State])
type SimpleTrans = (State, String, State) 


{-| The data constructor defines a FSM by the list of its transitions, list of initial
    states and list of final states.
    A transition/"delta function" is represented by a triplet which contains the initial 
    state of the transition, the characters of the alphabet being read on the input (more
    than one characters are needed for FSM -> RegEx transformation) and the list of final
    states.
    Since this data type may represent also a non-deterministic finite machine, the initial
    state is represented by the set of states rather than a single state.
-}
data FA = FA [Transition] Start Final
    deriving Show

-- | Gets the sorted set of all states of a FSM.

getStates :: FA -> [State]
getStates (FA [] _ _) = []
getStates (FA ((state, _, states):transitions) start final) = 
    sort $ [ y | y <- tryComplete]
    where other = getStates $ FA transitions start final    
          -- ^ Gets the states from the rest of the transitions in the given FSM.
          tryComplete  = (foldr (\x acc -> setSet x acc) other (state:states)) 
          -- ^ tryComplete is a function which tries to enlarge the inital set of states "other" by the states in the current transition.


-- getStatesToBeRemoved :: FA -> [State]
-- getStatesToBeRemoved fsm@(FA _ _ _) = [state | state <- getStates fsm]

getInitial :: FA -> [State]
getInitial (FA _ x _) = x

getFinal :: FA -> [State]
getFinal (FA _ _ x) = x

-- >>> getNonFinalStates (FA [(0, 'a', [1]), (1, 'b', [2]), (2, 'b', [3]), (3, 'a', [2,4])] [0] [1])
-- [2,3,4]
--

{-| Creates a new set of elements. The same set is returned if the element in the first argument already belongs to the set.
    Otherwise, new element is added to the set and this new set is returned. -}
setSet :: (Eq a) => a -> [a] -> [a]
setSet s [] = [s]
setSet s set =  if elem s set then
                    set
                else s:set

transitionSet :: Transition -> [Transition] -> [Transition]
transitionSet s [] = [s]
transitionSet t@(p,x,list_p) ts@((q,y,list_q):trans)
    | elem t ts = ts
    | p == q && x == y = let newList = foldr (\a acc -> setSet a acc) list_q list_p in trans ++ [(q, y, newList)]
    | otherwise = transitionSet t trans ++ [(q,y,list_q)]

-- | Decides, whether a given state is final in the given FSM.
isFinal :: State -> FA -> Bool
isFinal state fsm@(FA _  _ final) = elem state (getStates fsm) && elem state final

-- | Decides, whether a given state is initial in the given FSM.
isInitial :: State -> FA -> Bool
isInitial state fsm@(FA _ initial _) = elem state (getStates fsm) && elem state initial

apart :: FA -> State -> [State]
apart fsm@(FA _ initial _) f = [ state | state <- getStates fsm, state /= f, (not $ elem state initial)]


toSimpleTrans :: [Transition] -> [SimpleTrans]
toSimpleTrans complex = [(p,x,q)|(p,x,list_q) <- complex, q <- list_q]

getTransitions :: FA -> [Transition]
getTransitions (FA trans _ _) = trans

getSimpleTransitions :: FA -> [SimpleTrans]
getSimpleTransitions (FA trans _ _) = toSimpleTrans trans

getComplexTransitions :: [SimpleTrans] -> [Transition]
getComplexTransitions simple = [(p,x,[q])|(p,x,q) <- simple]

-- >>> getStates (FA [(0, 'a', [1]), (1, 'b', [2]), (2, 'b', [3]), (3, 'a', [2,4])])
-- [0,1,2,3,4]
--

-- >>> getStates (FA [(0, 'a', [3,1])])
-- [0,1,3]
--

-- >>> getStates (FA [(0, 'a', [5,1]), (1, 'b', [2,0]), (2, 'c', [1,0,2,3,4])])
-- [0,1,2,3,4,5]
--

