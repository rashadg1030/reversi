module Reversi.Types where

import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)

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

newtype GameM a = GameM (StateT GameState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GameState) -- MonadState GameState is important

class Monad m => Logger m where
  writeMoveMessage :: GameState -> Location -> m ()
  writePassMessage :: GameState -> m ()
  writeFailMessage :: GameState -> m ()
  writePrompt :: GameState -> m ()
  writePossibleMoves :: GameState -> m ()
  writeBoard :: GameState -> m ()
  writeFinalMessage :: Final -> m ()
  writeAny :: Show a => a -> m ()

class Monad m => Control m where
  getInput :: m Location

class Monad m => Generator m where
  randomLoc :: m Location
