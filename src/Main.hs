{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-}  

module Main where
  
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import System.Random (randomRIO)
import Text.Read hiding (get)
import Actions
import Board
import Types
import Control.Monad.State.Lazy

-- type Action = (Disc, Location)

-- type Frame = (Board, Action)

data GameState = GameState { currentDisc :: Disc, currentBoard :: Board, frames :: [GameState]  }
  deriving (Show, Eq)

newtype GameM a = GameM (StateT GameState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GameState) -- MonadState GameState is important

class Monad m => Logger m where
  writeMoveMessage :: Disc -> Location -> m ()
  writePassMessage :: Disc -> m ()
  writeFailMessage :: Disc -> m ()
  writePrompt :: Disc -> m ()
  writeFinalMessage :: Final -> m ()
  writePossibleMoves :: Disc -> Board -> m ()
  writeBoard :: Board -> m ()

class Monad m => Control m where
  getInput :: m Location

class Monad m => Generator m where 
  randomLoc :: m Location

instance Logger GameM where
  writeMoveMessage :: Disc -> Location -> GameM ()
  writeMoveMessage disc loc = liftIO . putStrLn $ (show disc) ++ " disc placed at " ++ (show loc) ++ "."

  writePassMessage :: Disc -> GameM ()
  writePassMessage disc = liftIO . putStrLn . ((show disc) ++) $ " passes..."

  writeFailMessage :: Disc -> GameM ()
  writeFailMessage d = liftIO . putStrLn $ (show d) ++ " made an invalid move."

  writePrompt :: Disc -> GameM ()
  writePrompt disc = liftIO . putStrLn . ((show disc) ++) $ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

  writeFinalMessage :: Final -> GameM ()
  writeFinalMessage (Win disc) = liftIO . putStrLn $ (show disc) ++ " won! " ++ (show . flipDisc $ disc) ++ " lost!"
  writeFinalMessage Tie        = liftIO . putStrLn $ "It's a tie!" 

  writePossibleMoves :: Disc -> Board -> GameM ()
  writePossibleMoves d = (liftIO . putStrLn . ("Possible Moves: "++) . show . possibleMoves d)

  writeBoard :: Board -> GameM ()
  writeBoard = liftIO . putBoard

instance Control GameM where
  getInput :: GameM Location
  getInput = do
    input <- liftIO $ getLine
    case (readMaybe input) :: Maybe Location of 
      (Just loc) -> return loc
      Nothing    -> do
        liftIO $ putStrLn "Invalid input. Try again."
        getInput
                
instance Generator GameM where
  randomLoc :: GameM Location
  randomLoc = do
    x <- liftIO $ randomRIO (0,7)
    y <- liftIO $ randomRIO (0,7)
    return (x, y)

main :: IO ()
main = runGameM stepGame

runGameM :: GameM a -> IO a
runGameM (GameM m) = evalStateT m startingState

stepGame :: (Logger m, Control m, MonadState GameState m) => m ()
stepGame = do
  gs <- get
  gameEnd
  let disc = currentDisc gs
  let board = currentBoard gs
  writeBoard board 
  case possibleMoves disc board of
    [] -> do
      writePassMessage disc
      modify changePlayer
      stepGame
    moves -> do
      writePrompt disc
      writePossibleMoves disc board
      loc <- getInput
      if elem loc moves then 
        do
          writeMoveMessage disc loc
          modify $ play loc
          stepGame 
      else
        if loc == ((-1), (-1)) then
          do 
            modify $ rewind
            stepGame
        else 
          do
            writeFailMessage disc
            stepGame

gameEnd :: (Logger m, MonadState GameState m) => m () -- Need (MonadState GameState m) constraint
gameEnd = do
  gs <- get 
  if noMoves gs then
    do 
      writeBoard $ currentBoard gs
      let final = getFinal gs
      writeFinalMessage final
  else return ()

-- Helper functions --
startingState :: GameState
startingState = GameState Black startingBoard []

-- For modifying GameState
play :: Location -> GameState -> GameState
play loc gs = addFrame new old
      where 
        new :: GameState 
        new = (changePlayer . (playDisc loc)) gs
        old :: GameState
        old = gs

addFrame :: GameState -> GameState -> GameState
addFrame (GameState disc board fs) old = GameState disc board (old:fs)  

rewind :: GameState -> GameState
rewind (GameState disc board []) = GameState disc board [] 
rewind (GameState _ _ (f:fs))    = GameState (currentDisc f) (currentBoard f) fs  

playDisc :: Location -> GameState -> GameState
playDisc loc (GameState disc board fs) = GameState disc (makeMove disc loc board) fs

changePlayer :: GameState -> GameState
changePlayer (GameState disc board fs) = GameState (flipDisc disc) board fs
 
noMoves :: GameState -> Bool
noMoves (GameState disc board _) = ((length $ possibleMoves disc board) == 0) && ((length $ possibleMoves (flipDisc disc) board) == 0)

getFinal :: GameState -> Final
getFinal (GameState _ board _) = if step31 == step32 then Tie else Win greater
  where
    step1   = Map.toList board 
    step2   = map snd step1
    step31  = length $ filter (\d1 -> d1 == White) step2
    step32  = length $ filter (\d2 -> d2 == Black) step2
    greater = if step31 > step32 then White else Black

-- Generate Random Game --
-- randomGame :: (Logger m, Generator m) => m ()
-- randomGame = do 
--   genRandomGame (State Black startingBoard)
    
-- genRandomGame :: (Logger m, Generator m) => State -> m ()
-- genRandomGame state@(State disc board) = do
--   gameEnd state
--   writeBoard board   
--   case possibleMoves disc board of
--     [] -> do
--       writePassMessage disc
--       let newState = (State (flipDisc disc) board)
--       genRandomGame newState 
--     _  -> do
--       loc <- genLoc state
--       writeMoveMessage disc loc 
--       let newState = (State (flipDisc disc) (makeMove disc loc board))
--       genRandomGame newState  
                   
-- genLoc :: Generator m => State -> m (Int, Int)
-- genLoc state@(State disc board) = do
--   let possible = possibleMoves disc board
--   loc <- randomLoc
--   if elem loc possible then return loc else genLoc state         