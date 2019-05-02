module Types where

import Data.Map.Strict (Map)

data Disc = Black | White
    deriving (Show, Eq, Ord) --Ord??

data Final = Win Disc | Tie 
  deriving (Show, Eq)
  
type Cell = Maybe Disc

type Location = (Int, Int)

data Move = Pass | Move Location
  deriving (Show, Eq, Ord) -- Ord??

type Board = Map Location Disc

data GameState = GameState { getDisc :: Disc, getBoard :: Board, getMove :: Move, getFrames :: [GameState]  }
  deriving (Show, Eq, Ord)