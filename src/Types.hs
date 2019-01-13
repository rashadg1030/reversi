module Types where

import Data.Map.Strict (Map)
--import Control.Monad.Trans

data State = State Disc Board

data Disc = Black | White
    deriving (Show, Eq)

data Final = Win Disc | Tie 
  
type Cell = Maybe Disc

type Location = (Int, Int)

type Board = Map Location Disc
