{-# LANGUAGE InstanceSigs #-}

module Reversi.GameTree where

import Reversi.Types
import Reversi.Board
import qualified Data.Map as Map
import Reversi.Actions
import Reversi.Heuristic
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

nextTurn :: GameState -> Location -> GameState -- Might be wise to make this so that it returns a RoseTree GameState
nextTurn gs loc = case plausibleMoves gs of
                    [] -> pass gs
                    _  -> play loc gs

playAll :: GameState -> [GameState]
playAll gs = fmap (nextTurn gs) moveList
  where 
    moveList = plausibleMoves gs

gameStateToNode :: GameState -> RoseTree GameState
gameStateToNode gs = Node gs []

-- For generating a game tree from a given gamestate !!!!!!!!!!!!!!!!!!!!!!
genGameTree :: Int -> GameState -> RoseTree GameState
genGameTree depth gs
  | depth <= 0 = gameStateToNode gs 
  | otherwise  = (Node gs (genGameTree (depth - 1) <$> (playAll gs)))

-- For testing gameTree generation
testGenGameTree :: Int -> RoseTree GameState
testGenGameTree depth = genGameTree depth startingState

evalGameState :: GameState -> (Int, GameState) -- The maximizing player is whoever plays next from the current gameState
evalGameState gs = (evalBoard (getDisc gs) (getBoard gs), gs)

runMinmax :: GameState -> Move
runMinmax gs = getMove . secondToLast . getFrames . snd . minmax $ gameTree
  where
    gameTree = genGameTree 3 gs 
    minmax :: RoseTree GameState -> (Int, GameState)
    minmax (Node gs [])       = evalGameState gs
    minmax (Node gs children) = case getDisc gs of 
                                  White -> minimum $ minmax <$> children
                                  Black -> maximum $ minmax <$> children 

refresh :: GameState -> GameState
refresh gs = gs {getFrames = []}

moveToLoc :: Move -> Location
moveToLoc move = case move of
                   Move loc -> loc
                   Pass     -> error "The AI wants to pass..."
                   
secondToLast :: [a] -> a
secondToLast []     = error "Empty list."
secondToLast [x,_]  = x
secondToLast (_:xs) = secondToLast xs




