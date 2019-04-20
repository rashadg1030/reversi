{-# LANGUAGE InstanceSigs #-}

module GameTree where

import Types
import Board
import qualified Data.Map as Map
import Actions
import Heuristic
import Text.Pretty.Simple (pPrint)

data RoseTree a = Node a [RoseTree a] -- Should be monoid??
  deriving (Show, Eq)

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (Node x [])    = (Node (f x) [])
  fmap f (Node x roses) = (Node (f x) (map (fmap f) roses))

countNodes :: RoseTree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ xs) = 1 + (sum $ map countNodes xs) 

-- For generating a game tree !!!!!!!!!!!!!!!!!!!!!!
genGameTree :: Int -> RoseTree GameState -> RoseTree GameState
genGameTree depth rt@(Node gs _)
  | depth <= 0 = rt 
  | otherwise  = (Node gs (genGameTree (depth - 1) <$> (gameStateToNode <$> playAll gs)))

gameStateToNode :: GameState -> RoseTree GameState
gameStateToNode gs = Node gs []

nextTurn :: GameState -> Location -> GameState -- Might be wise to make this so that it returns a RoseTree GameState
nextTurn gs loc = case plausibleMoves gs of
                    [] -> pass gs
                    _  -> play loc gs

playAll :: GameState -> [GameState]
playAll gs = fmap (nextTurn gs) moveList
  where 
    moveList = plausibleMoves gs
--- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- For testing gameTree generation
testGenGameTree :: Int -> RoseTree GameState
testGenGameTree depth = genGameTree depth testSeed

testSeed :: RoseTree GameState
testSeed = toSeed startingState
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- To start the algorithm, we must first take the game state and 
-- turn it to a beginning gameState node
toSeed :: GameState -> RoseTree GameState
toSeed gs = Node ( gs { getMove = Begin, getFrames = [] } ) []

data Result = Result { gs :: GameState, score' :: Int }
  deriving (Show, Eq)

instance Ord Result where
  (<=) Result{ gs = g1, score' = s1 } Result{ gs = g2, score' = s2 } = case getMove g1 of
    Begin  -> case getMove g2 of
                Begin  -> s1 <= s2
                Pass   -> True
                Move _ -> True
    Pass   -> case getMove g2 of
                Begin  -> False 
                Pass   -> s1 <= s2
                Move _ -> True
    Move _ -> case getMove g2 of
                Begin  -> False
                Pass   -> False
                Move _ -> s1 <= s2

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

extractMove :: GameState -> Move
extractMove gs = answer
  where
    answer = undefined
    peek :: GameState -> Bool
    peek = undefined
    resultFrames = getFrames result
    result       = runMinmax gs

runMinmax :: GameState -> GameState
runMinmax g = gs $ minmax (getDisc g) (genGameTree 3 $ toSeed g) 
 
-- Almost there but not good because the result is being propgated up the tree
-- so you don't get a move you can use.
minmax :: Disc -> RoseTree GameState -> Result
minmax d (Node gs [])       = Result gs (evalBoard d $ getBoard gs)
minmax d (Node gs children) = maximum $ (minmax $ flipDisc d) <$> children
    
-- Instead of doing everything step by step, do a everything in one go. evalLeaves should
-- maximum and minimum there.

-- -- Should be false
-- ordTest = Result { move = Move (1,0), score' = 3 } <= Result { move = Begin, score' = 100 }

-- -- should be true
-- ordTest1 = Result { move = Move (1,0), score' = 3 } <= Result { move = Move (1,0), score' = 100 }

-- -- should be true
-- ordTest2 = Result { move = Pass, score' = 34 } <= Result { move = Move (1,0), score' = 0 }


