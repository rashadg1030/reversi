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

runMinmax :: GameState -> Location
runMinmax gs = minmax (getDisc gs) (genGameTree 3 $ toSeed gs) 

minmax :: Disc -> RoseTree GameState -> Location
minmax (Node gs []) = evalBoard ai board
minmax (Node gs )   = 


    
-- Instead of doing everything step by step, do a everything in one go. evalLeaves should
-- maximum and minimum there.


