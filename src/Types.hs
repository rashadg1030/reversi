module Types where

import Data.Map.Strict (Map)

data State = State Disc Board

data Disc = Black | White
    deriving (Show, Eq)

data Final = Win Disc | Tie 
  
type Cell = Maybe Disc

type Location = (Int, Int)

type Board = Map Location Disc

data GameState = GameState { getDisc :: Disc, getBoard :: Board, getFrames :: [GameState]  }
  deriving (Show, Eq)
