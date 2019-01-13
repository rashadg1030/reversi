{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
  
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.IO.Class
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Data.Maybe
import Text.Read
import Actions
import Board
import Types 

newtype GameM a = GameM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

{-
Abstract the print-like functions with a monadic type class named Logger. Give GameM an instance of logger with the liftIO definitions
The idea is to abstract over IO functions
-}

class Monad m => Logger m where
  writeMessage :: String -> m ()
  writeBoard :: Board -> m ()

class Monad m => Control m where
  getInput :: m String


instance Logger (GameM) where
  writeMessage :: String -> GameM ()
  writeMessage = liftIO . putStrLn

  writeBoard :: Board -> GameM ()
  writeBoard = liftIO . putBoard

instance Control (GameM) where
  getInput :: GameM String
  getInput = liftIO $ getLine

main :: IO ()
main = runGameM play

runGameM :: GameM a -> IO a
runGameM (GameM io) = io 

play :: GameM ()
play = stepGame startingState

stepGame :: State -> GameM ()
stepGame state@(State disc board) = do
  gameEnd state
  writeBoard board

  case possibleMoves disc board of
    []     -> do
                writeMessage $ passMessage disc
                stepGame (State (flipDisc disc) board)
    moves  -> do
                writeMessage $ moveMessage disc
                input <- getInput
                  
                case readMaybe input of
                  Nothing    -> do
                                  writeMessage "Invalid input. Try Again."
                                  stepGame state
                  (Just loc) -> do
                                  if (elem loc moves) then
                                    do   
                                      writeMessage "Valid location."
                                      stepGame (State (flipDisc disc) (makeMove disc loc board))
                                  else
                                    do 
                                      writeMessage "Can't make that move. Try Again."
                                      stepGame state

randomGame :: GameM ()
randomGame = do 
  genRandomGame (State Black startingBoard)

genRandomGame :: State -> GameM ()
genRandomGame state@(State disc board) = do
  gameEnd state
  writeBoard board   
  case possibleMoves disc board of
    []     -> do
                writeMessage "#PASS#"
                let newState = (State (flipDisc disc) board)
                genRandomGame newState 
    (x:xs) -> do
                writeMessage (show disc)
                loc <- genLoc state
                writeMessage $ "Move: " ++ (show loc) 
                let newState = (State (flipDisc disc) (makeMove disc loc board))
                genRandomGame newState  

genLoc :: State -> GameM (Int, Int)
genLoc state@(State disc board) = do
  let possible = possibleMoves disc board
  x <- liftIO $ randomRIO (0,7) 
  y <- liftIO $ randomRIO (0,7) 
  if elem (x, y) possible then return (x,y) else genLoc state         
                
gameEnd :: State -> GameM ()
gameEnd state@(State disc board) = 
  if noMoves state then
    do 
      writeBoard board
      if (isWinner Black board) then
        writeMessage "Black won! White lost!"
      else 
        writeMessage "White won! Black lost!"
  else return ()
-- Helper functions --

startingState :: State
startingState = (State Black startingBoard)

noMoves :: State -> Bool
noMoves state@(State disc board) = ((length $ possibleMoves disc board) == 0) && ((length $ possibleMoves (flipDisc disc) board) == 0)

isWinner :: Disc -> Board -> Bool
isWinner disc board = answer
  where
    step1  = Map.toList board 
    step2  = map snd step1
    step31 = filter (\d1 -> d1 == disc) step2
    step32 = filter (\d2 -> d2 == (flipDisc disc)) step2
    answer = (length step31) > (length step32)

passMessage :: Disc -> String
passMessage disc = (show disc) ++ " passes..."  
    
moveMessage :: Disc -> String
moveMessage disc = (show disc) ++ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."