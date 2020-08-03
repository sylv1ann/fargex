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


{-| The data constructor defines a FSM by the list of its transitions, list of initial
    states and list of final states.
    A transition/"delta function" is represented by a triplet which contains the initial 
    state of the transition, the character of the alphabet being read on the input and the
    list of final states.
    Since this data type may represent also a non-deterministic finite machine, the initial
    state is represented by the set of states rather than a single state.
-}
data FA = FA [(State, Char, [State])] Start Final
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


getNonFinalStates :: FA -> [State]
getNonFinalStates fsm@(FA _ initial final) = [state | state <- getStates fsm, not (elem state initial) && not (elem state final)]

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

-- | Decides, whether a given state is final in the given FSM.
isFinal :: State -> FA -> Bool
isFinal state fsm@(FA _  _ final) = elem state (getStates fsm) && elem state final

-- | Decides, whether a given state is initial in the given FSM.
isInitial :: State -> FA -> Bool
isInitial state fsm@(FA _ initial _) = elem state (getStates fsm) && elem state initial

-- >>> getStates (FA [(0, 'a', [1]), (1, 'b', [2]), (2, 'b', [3]), (3, 'a', [2,4])])
-- [0,1,2,3,4]
--

-- >>> getStates (FA [(0, 'a', [3,1])])
-- [0,1,3]
--

-- >>> getStates (FA [(0, 'a', [5,1]), (1, 'b', [2,0]), (2, 'c', [1,0,2,3,4])])
-- [0,1,2,3,4,5]
--
