module Types where

import Data.Map.Strict (Map)

data State = State Disc Board

data Disc = Black | White
    deriving (Show, Eq)

data Final = Win Disc | Tie 
  
type Cell = Maybe Disc

type Location = (Int, Int)

-- data Input = Start | Location   -- Confusing lol

-- Should be Pass instead of start. That fixes a problem with Passing
data Move = Begin | Pass | Move Location
  deriving (Show, Eq)

type Board = Map Location Disc

data GameState = GameState { getDisc :: Disc, getBoard :: Board, getMove :: Move, getFrames :: [GameState]  }
  deriving (Show, Eq)

-- Either add a new type to GameState record, or just use prevMove to get the previous move.