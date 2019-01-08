{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

startingState :: State
startingState = (State Black startingBoard)

passMessage :: Disc -> String
passMessage disc = (discName disc) ++ " passes..."  
      
moveMessage :: Disc -> String
moveMessage disc = (discName disc) ++ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

discName :: Disc -> String
discName White = "White"
discName Black = "Black"

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

main :: IO ()
main = do 
  runGame startingState                                      
                
runGame :: State -> IO ()
runGame state@(State disc board) = do
    gameEnd state
    putBoard board

    case possibleMoves disc board of
        []     -> do
                    putStrLn $ passMessage disc
                    runGame (State (flipDisc disc) board)
        (x:xs) -> do
                    putStrLn $ moveMessage disc
                    input <- getLine
                    
                    case readMaybe input of
                        Nothing    -> do
                                        putStrLn "Invalid input. Try Again."
                                        runGame state
                        (Just loc) -> do
                                        let possible = possibleMoves disc board
                                        if (elem loc possible) then
                                            do   
                                                putStrLn "Valid location."
                                                runGame (State (flipDisc disc) (makeMove disc loc board))
                                        else
                                            do 
                                                putStrLn "Can't make that move. Try Again."
                                                runGame state
                
randomGame :: IO ()
randomGame = do
  genRandomGame (State Black startingBoard)

genRandomGame :: State -> IO ()
genRandomGame state@(State disc board) = do
  gameEnd state
  putBoard board   
  case possibleMoves disc board of
    []     -> do
                putStrLn "#PASS#"
                let newState = (State (flipDisc disc) board)
                genRandomGame newState 
    (x:xs) -> do
                print disc
                loc <- genLoc state
                putStr "Move: "
                print loc
                let newState = (State (flipDisc disc) (makeMove disc loc board))
                genRandomGame newState 

genLoc :: State -> IO (Int, Int)
genLoc state@(State disc board) = do
    let possible = possibleMoves disc board
    x <- randomRIO (0,7)
    y <- randomRIO (0,7)
    if elem (x, y) possible then return (x,y) else genLoc state
                
gameEnd :: State -> IO ()
gameEnd state@(State disc board) = 
  if noMoves state then
    do 
      putBoard board
      if (isWinner Black board) then
        putStrLn "Black won! White lost!"
      else 
        putStrLn "White won! Black lost!"
      exitSuccess
  else (return ())