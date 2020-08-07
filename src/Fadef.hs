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
data FSM = FSM [Transition] Start Final
    deriving (Show, Read)

-- | Gets the sorted set of all states of a FSM.
getStates :: FSM -> [State]
getStates (FSM [] _ _) = []
getStates (FSM ((state, _, states):transitions) start final) = 
    sort $ [ y | y <- tryComplete]
    where other = getStates $ FSM transitions start final    
          -- ^ Gets the states from the rest of the transitions in the given FSM.
          tryComplete  = (foldr (\x acc -> setSet x acc) other (state:states)) 
          -- ^ tryComplete is a function which tries to enlarge the inital set of states "other" by the states in the current transition.

-- | Basic check whether the FSM is defined correctly.
isCorrectFSM :: FSM -> Bool
isCorrectFSM fsm@(FSM trans initial final) = 
    let states = getStates fsm
    in  all (\x -> elem x states) initial && 
        all (\x -> elem x states) final   &&
        all (\(p,_,list_p) -> elem p states && (all (\x -> elem x states) list_p)) trans
 
-- | Returns the list of initial states for given FSM (since the FSM can be non-deterministic)
getInitial :: FSM -> [State]
getInitial (FSM _ x _) = x

-- | Returns the list of final states for given FSM
getFinal :: FSM -> [State]
getFinal (FSM _ _ x) = x

-- | Decides, whether a given state is initial in the given FSM.
isInitial :: State -> FSM -> Bool
isInitial state fsm@(FSM _ initial _) = elem state (getStates fsm) && elem state initial

-- | Decides, whether a given state is final in the given FSM.
isFinal :: State -> FSM -> Bool
isFinal state fsm@(FSM _  _ final) = elem state (getStates fsm) && elem state final

{-| Creates a new set of elements. The same set is returned if the element in the first argument already belongs to the set.
    Otherwise, new element is added to the set and this new set is returned. -}
setSet :: (Eq a) => a -> [a] -> [a]
setSet s [] = [s]
setSet s set =  if elem s set then
                    set
                else s:set

{- | setSet equivalence for transitions:
    - if a triplet already exists, nothing is added i.e. (1,"a",[2]) in [(1,"a",[2]), (1,"b",[3])] -> [(1,"a",[2]), (1,"b",[3])]
    - if given transition already exists, but can be completed: (1,"a",[4]) [(1,"a",[2]), (1,"b",[3])] -> [(1,"a",[4,2]), (1,"b",[3])]
    - if given transition does not exist yet, then it is added: (1,"c",[2,3]) [(1,"a",[2]), (1,"b",[3])] -> [(1,"c",[2,3]),(1,"a",[2]), (1,"b",[3])]
-}
transitionSet :: Transition -> [Transition] -> [Transition]
transitionSet s [] = [s]
transitionSet t@(p,x,list_p) ts@((q,y,list_q):trans)
    | elem t ts = ts -- the triplet already exists
    | p == q && x == y = let newList = foldr (\a acc -> setSet a acc) list_q list_p in trans ++ [(q, y, newList)] -- the order of triplets does not need to be the same in the result set
    | otherwise = transitionSet t trans ++ [(q,y,list_q)] -- completely new triplet -> added as it is

-- >>> transitionSet (1,"a",[2]) [(1,"a",[2]), (1,"b",[3])]
-- [(1,"a",[2]),(1,"b",[3])]
--

-- >>> transitionSet (1,"a",[4]) [(1,"a",[2]), (1,"b",[3])]
-- [(1,"b",[3]),(1,"a",[4,2])]
--

-- >>> transitionSet (1,"c",[2,3]) [(1,"a",[2]), (1,"b",[3])]
-- [(1,"c",[2,3]),(1,"b",[3]),(1,"a",[2])]
--

-- | Returns the list of all states of a FSM except the given state f and the set of initial states
apart :: FSM -> State -> [State]
apart fsm@(FSM _ initial _) f = [ state | state <- getStates fsm, state /= f, (not $ elem state initial)]

{- | Transforms the list of (State, String, [State]) to list of (State, String, State)
     where first two elements are same in both triplets, but the third State is element
     of [State] from the first triplet
-}
toSimpleTrans :: [Transition] -> [SimpleTrans]
toSimpleTrans complex = [(p,x,q)|(p,x,list_q) <- complex, q <- list_q]

-- | Returns the list of all transitions for given FSM.
getTransitions :: FSM -> [Transition]
getTransitions (FSM trans _ _) = trans

-- | Returns the list of all transitions for given FSM transformed to the list of simple transitions.
getSimpleTransitions :: FSM -> [SimpleTrans]
getSimpleTransitions (FSM trans _ _) = toSimpleTrans trans

-- | Transforms the list of simple (State, String, String) transitions to (not neccesarilly smallest) set of (State, String, [State]) transitions. 
getComplexTransitions :: [SimpleTrans] -> [Transition]
getComplexTransitions simple = [(p,x,[q])|(p,x,q) <- simple]

-- >>> getStates (FSM [(0, "a", [1]), (1, "b", [2]), (2, "b", [3]), (3, "a", [2,4])] [1] [4])
-- [0,1,2,3,4]
--

-- >>> getStates (FSM [(0, "a", [3,1])] [0] [5])
-- [0,1,3]
--

-- >>> getStates (FSM [(0, "a", [5,1]), (1, "b", [2,0]), (2, "c", [1,0,2,3,4])] [0] [4])
-- [0,1,2,3,4,5]
--
