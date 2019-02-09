{-# LANGUAGE InstanceSigs #-}

module GameTree where

import Types
-- import Board
import Actions

data RoseTree a = Node a [RoseTree a] 
    deriving (Show, Eq)

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (Node x [])    = (Node (f x) [])
    fmap f (Node x roses) = (Node (f x) (map (fmap f) roses))

-- instance Applicative RoseTree where
--     pure :: a -> RoseTree a
--     pure x = Node x []
    -- Maybe No Applicative??
    -- (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
    -- (<*>) (Node f []) (Node x _)  = Node (f x) []
    -- (<*>) (Node f _) (Node x [])  = Node (f x) []
    -- (<*>) (Node f fs) (Node x xs) = Node (f x) (listApply ((<*>) fs) xs)
    -- fs :: [RoseTree (a -> b)]
    -- xs :: [RoseTree a]
    --     where 
    --         answer :: RoseTree b
    --         answer = (List.<*>) (fs <*>) xs    
            
-- -- How to return [a] if emptyList of functions is passed in
-- listApply :: [(a -> b)] -> [a] -> [b]
-- listApply [] _  = []
-- listApply _  [] = []
-- listApply fs xs = [ f x | f <- fs, x <- xs] 

gameStateToNode :: GameState -> RoseTree GameState
gameStateToNode gs = Node gs []

playAll :: GameState -> [GameState]
playAll gs = fmap (flip play $ gs) moveList
    where 
        moveList = plausibleMoves gs

genGameTree :: Int -> RoseTree GameState -> RoseTree GameState
genGameTree d seed
    | d <= 0    = seed 
    | otherwise = case seed of
                    Node gs []       -> genGameTree d (Node gs (gameStateToNode <$> playAll gs)) 
                    Node gs children -> Node gs (map (genGameTree (d-1)) children)
                    -- children :: [RoseTree GameState]
                    -- gs :: GameState
                    -- d-1 one on either path works??

