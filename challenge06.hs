import DFA    -- hidden module, defines DFA types as in worksheet 3
import RunDFA -- hidden module, contains a completed DFA emulator
import VisDFA -- hidden module, contains DFA visualisation tools
import EqDFA  -- hidden module, required for testing purposes

import Data.List
import Debug.Trace

type Transition = ((Int, Char), Int)
type Transitions = [((Int, Char), Int)]

-- **************************************************************************
-- from wroksheet 3
-- We introduce a special 'dead state'/'reject state', to be used
-- when the given transition function hasn't been completed.
-- Since a well-formed DFA's states must be non-negative integers,
-- using -1 will suffice: Any future transitions will also be
-- missing from the transition function, and this state will not be
-- an accept state.
deadState :: State
deadState = -1

-- The DFA steps through successive configurations.  A configuration 
-- is a pair (state, remaining input).
type Configuration = (State, Input)

step :: [Transn] -> Configuration -> Configuration
step trans (cur_int, chars) = if looked == Nothing then
                                  (deadState, rest)
                              else if cur_int == deadState then
                                  (deadState, rest)
                              else
                                  (next, rest)
    where
        input = head chars
        rest = tail chars
        looked = lookup (cur_int, input) trans
        next = snd $ head $ filter (\x -> ((fst (fst x)) == cur_int) && 
                                    ((snd (fst x)) == input)) trans
-- **************************************************************************

-- put your solution to challenge 6 here:
-- minized DFA
multiples :: Int -> DFA
multiples n = (statesNew, alphabets, transitionsNew, startSate, acceptStates)
    where
        (states, alphabets, transitions, startSate, acceptStates) = multiples' n
        (statesNew, transitionsNew) = recurMerge states acceptStates transitions n

multiples' n = (states, alphabets, transitions, startSate, acceptStates)
    where
        states = [0..(n-1)]
        alphabets = "01"
        transitions = generateTransitions 0 n
        startSate = 0
        acceptStates = [0]

-- **************************************************************************
-- minizing DFA
checkCanMerge :: Transitions -> [Int] -> (Int, Int) -> (Bool, ((Int, Int), (Int, Int)))
checkCanMerge trans acceptStates (s1, s2)
    -- has same destination for one step, different, neither is accepted state
    = (
        ((s1ZeroStepState == s2ZeroStepState) && (s1OneStepState == s2OneStepState) && 
        (s1 /= s2) && (not $ s1 `elem` acceptStates) && (not $ s2 `elem` acceptStates)), 
        (
         (s1, s2),
         (s1ZeroStepState, s1OneStepState)
        )
      )
        where
            s1ZeroStep@(s1ZeroStepState, _) = step trans (s1, "0")
            s1OneStep@(s1OneStepState, _) = step trans (s1, "1")
            s2ZeroStep@(s2ZeroStepState, _) = step trans (s2, "0")
            s2OneStep@(s2OneStepState, _) = step trans (s2, "1")

-- return can merged [((state1, state2), (0 transition destination, 1 transition destination))]
getCanMerge :: Transitions -> [Int] -> Int -> [((Int, Int), (Int, Int))]
getCanMerge trans acceptStates n = map snd canMerge
    where
        allChecks = [(x, y) | x <- [0..(n-1)], y <- [x..(n-1)]]
        allChecked = map (checkCanMerge trans acceptStates) allChecks
        canMerge = filter (\x -> fst x) allChecked

-- for deleted state, let arrow attatch on it changed to new state
mergedStateFromToNewStateFrom :: Transitions -> Int -> Int -> Transitions
mergedStateFromToNewStateFrom [] _ _ = []
mergedStateFromToNewStateFrom (x:xs) old new = tran:(mergedStateFromToNewStateFrom xs old new)
    where
        tran = if snd x == old then
                   (fst x, new)
               else
                   x

mergeState :: ((Int, Int), (Int, Int)) -> [Int] -> Transitions -> ([Int], Transitions)
mergeState ((s1, s2), (to0, to1)) state trans
    = ((delete s2 state), newTrans')
        where
            -- old state: s2, new state: s1
            -- delete old states out arrows
            filteredTrans = filter (\x -> not (fst (fst x) `elem` [s1, s2])) trans
            curStates = [s1, s2]
            -- new state 0, 1 out arrow, ensure out self arrow point to new state
            to0' = if to0 `elem` curStates then
                       s1
                   else
                       to0
            to1' = if to1 `elem` curStates then
                       s1
                   else
                       to1
            zero = ((s1, '0'), to0')
            one  = ((s1, '1'), to1')
            newTrans = filteredTrans ++ [zero, one]
            -- see function def
            newTrans' = mergedStateFromToNewStateFrom newTrans s2 s1

recurMerge :: [Int] -> [Int] -> Transitions -> Int -> ([Int], Transitions)
recurMerge state acceptStates trans n = 
        if trace (show canMerge) (length canMerge == 0) then
            (state, trans)
        else
            recurMerge stateNew acceptStates transNew (length state)
    where
        canMerge = getCanMerge trans acceptStates n
        (stateNew, transNew) = mergeState (head canMerge) state trans

-- **************************************************************************
-- not minized DFA
generateTransitions :: Int -> Int -> Transitions
generateTransitions k n
    | k == n = []
    | otherwise = zero:one:(generateTransitions (k+1) n)
        where
            zeroTo = ((n * k + k) * 2) `mod` n
            zero = ((k, '0'), zeroTo)
            oneTo = ((n * k + k) * 2 + 1) `mod` n
            one = ((k, '1'), oneTo)

-- [((0,'0'),0),((0,'1'),1),((1,'0'),2),((1,'1'),3),((2,'0'),4),((2,'1'),5),((3,'0'),0),((3,'1'),1),((4,'0'),2),((4,'1'),3),((5,'0'),4),((5,'1'),5)]
-- getCanMerge [((0,'0'),0),((0,'1'),1),((1,'0'),2),((1,'1'),3),((2,'0'),4),((2,'1'),5),((3,'0'),0),((3,'1'),1),((4,'0'),2),((4,'1'),3),((5,'0'),4),((5,'1'),5)] [0] 6
-- visDFA $ multiples 6
