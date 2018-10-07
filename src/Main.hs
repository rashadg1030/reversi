module Main where

import qualified Data.Map.Strict as Map
import Control.Monad
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Actions
import Board
import Types 

main :: IO ()
main = do
  let startingState = (State Black startingBoard)
  runGame startingState 

runGame :: State -> IO ()
runGame state@(State disc board) = forever $ do
  gameEnd state
  putBoard board   
  if (possibleMoves disc board == []) then
    do
      putStr "#PASS#\n"
      (return (State (flipDisc disc) board)) >>= runGame
  else
    do
      print disc
      loc <- genLoc state
      putStr "Move: "
      print loc
      (return (State (flipDisc disc) (makeMove disc loc board))) >>= runGame

genLoc :: State -> IO (Int, Int)
genLoc state@(State disc board) = do
                                   x <- randomRIO (0,7)
                                   y <- randomRIO (0,7)
                                   if elem (x, y) possible then
                                    return (x,y)
                                   else genLoc state
                                where 
                                  possible = possibleMoves disc board

gameEnd :: State -> IO ()
gameEnd state@(State disc board) = 
  if noMoves state then
    do putBoard board
       if (isWinner Black board) then
        putStrLn "Black won! White lost!"
       else putStrLn "White won! Black lost!"
       putStrLn "Better luck next time!"
       exitSuccess
  else return () 

noMoves :: State -> Bool
noMoves state@(State disc board) = ((length $ possibleMoves disc board) == 0) && ((length $ possibleMoves (flipDisc disc) board) == 0)

isWinner :: Disc -> Board -> Bool
isWinner disc board = answer
                    where
                      step1 = Map.toList board 
                      step2 = map snd step1
                      step31 = filter (\d1 -> d1 == disc) step2
                      step32 = filter (\d2 -> d2 == (flipDisc disc)) step2
                      answer = (length step31) > (length step32) 