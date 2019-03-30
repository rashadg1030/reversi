module Types where

import Data.Map.Strict (Map)

data Disc = Black | White
    deriving (Show, Eq)

data Final = Win Disc | Tie 
  deriving (Show, Eq)
  
type Cell = Maybe Disc

type Location = (Int, Int)

data Move = Begin | Pass | Move Location
  deriving (Show, Eq)

type Board = Map Location Disc

data GameState = GameState { getDisc :: Disc, getBoard :: Board, getMove :: Move, getFrames :: [GameState]  }
  deriving (Show, Eq)