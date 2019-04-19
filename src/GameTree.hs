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

-- apply evaluation to leaf nodes
-- all other nodes have a score of 0
-- the first arguement is the maximizing
-- Odd number tree must be passed in
evalLeaves :: Disc -> RoseTree GameState -> RoseTree (GameState, Int)
evalLeaves maxDisc rt@(Node root _) = case rt of
                                        (Node gs [])       -> Node (gs, (evalBoard maxDisc board)) []
                                        (Node gs children) -> Node (gs, 0) (evalLeaves maxDisc <$> children)
  where
    board = getBoard root

evalLeaves' :: RoseTree GameState -> RoseTree (GameState, Int)
evalLeaves' rt@(Node gs _) = evalLeaves maxDisc $ genGameTree 3 seed  
  where
    maxDisc = getDisc gs 
    seed    = toSeed rt

toSeed :: RoseTree GameState -> RoseTree GameState
toSeed (Node gs _) = Node (toBegin gs) []
  where 
    toBegin :: GameState -> GameState
    toBegin gs = gs { getMove = Begin, getFrames = [] }

-- !!!!!!Doesn't work good
-- Takes max Player
instance Ord (GameState, Int) where
  (<=) (_, x) (_, y) = (<=) x y 

findBestChild :: Disc -> RoseTree (GameState, Int) -> (GameState, Int)
findBestChild maxDisc rt = case rt of
                             (Node gsInt [])       -> gsInt
                             (Node gsInt children) -> if colorOf gsInt == maxDisc then
                                                        maximum children
                                                      else
                                                        minimum children
  where
    colorOf (gs, _) = getDisc gs
    
-- Instead of doing everything step by step, do a everything in one go. evalLeaves should
-- maximum and minimum there.
                                                      

getBestMove :: RoseTree (Move, Int) -> IO Location
getBestMove rt = undefined

gsIntTreeToMoveIntTree :: RoseTree (GameState, Int) -> RoseTree (Move, Int)
gsIntTreeToMoveIntTree rt = someFunc <$> rt
  where
    someFunc :: (GameState, Int) -> (Move, Int)
    someFunc (gs, x) = (getMove gs, x)

testGenGameTree :: Int -> RoseTree GameState
testGenGameTree depth = genGameTree depth testSeed

testSeed :: RoseTree GameState
testSeed = gameStateToNode startingState

-- newtype MoveScore = MoveScore (Move, Int)
--     deriving (Eq, Show)

-- instance Ord MoveScore where
--     (<=) (MoveScore (Move _, x)) (MoveScore (Move _, y)) = x <= y
--     (<=) (MoveScore (move1, _)) (MoveScore (move2, _))   = move1 <= move2 



-- function minimax(node, depth, maximizingPlayer) is
--     if depth = 0 or node is a terminal node then
--         return the heuristic value of node
--     if maximizingPlayer then
--         value := −∞
--         for each child of node do
--             value := max(value, minimax(child, depth − 1, FALSE))
--         return value
--     else (* minimizing player *)
--         value := +∞
--         for each child of node do
--             value := min(value, minimax(child, depth − 1, TRUE))
--         return value\

-- minmax :: Disc -> Int -> RoseTree GameState -> MoveScore
-- minmax disc depth (Node gs children) 
--   | length children == 0 || depth == 0 = MoveScore (getMove gs, evalBoard disc (getBoard gs))
--   | otherwise = if getDisc gs == disc then
--                   maximum $ minmax (flipDisc disc) (depth-1) <$> children
--                 else
--                   minimum $ minmax (disc) (depth-1) <$> children

--minmax :: Disc -> Int -> RoseTree GameState -> (Int, Int)
--minmax 