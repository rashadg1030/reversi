module Types where

import Data.Map.Strict (Map)
--import Control.Monad.Trans

data State = State Disc Board

data Disc = Black | White
    deriving (Show, Eq)
  
type Cell = Maybe Disc

type Location = (Int, Int)

type Board = Map Location Disc
