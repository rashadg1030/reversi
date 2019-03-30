{-# LANGUAGE InstanceSigs #-}

module GameTree where

import Types
import Board
import qualified Data.Map as Map
import Actions
import Heuristic (evalBoard)
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

gameStateToNode :: GameState -> RoseTree GameState
gameStateToNode gs = Node gs []


nextTurn :: GameState -> Location -> GameState
nextTurn gs loc = case plausibleMoves gs of
                    [] -> pass gs
                    _  -> play loc gs

playAll :: GameState -> [GameState]
playAll gs = fmap (nextTurn gs) moveList
    where 
        moveList = plausibleMoves gs

genGameTree :: Int -> RoseTree GameState -> RoseTree GameState
genGameTree depth rt@(Node gs _)
    | depth <= 0 = rt 
    | otherwise  = (Node gs (genGameTree (depth - 1) <$> (gameStateToNode <$> playAll gs)))

seed :: RoseTree GameState
seed = gameStateToNode startingState

testGenGameTree :: Int -> RoseTree GameState
testGenGameTree depth = genGameTree depth seed