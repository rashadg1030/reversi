{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

main :: IO ()
main = undefined

-- Start --
newtype GameM a = GameM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

play :: GameM ()
play = do
  runGame startingState

runGame :: State -> GameM ()
runGame state@(State disc board) = do
  gameEnd state
  writeBoard board

  case possibleMoves disc board of
    []     -> do
                liftIO $ putStrLn $ passMessage disc
                runGame (State (flipDisc disc) board)
    (x:xs) -> do
                liftIO $ putStrLn $ moveMessage disc
                input <- liftIO $ getLine
                  
                case readMaybe input of
                  Nothing    -> do
                                  liftIO $ putStrLn "Invalid input. Try Again."
                                  runGame state
                  (Just loc) -> do
                                  let possible = possibleMoves disc board
                                  if (elem loc possible) then
                                    do   
                                      liftIO $ putStrLn "Valid location."
                                      runGame (State (flipDisc disc) (makeMove disc loc board))
                                  else
                                    do 
                                      liftIO $ putStrLn "Can't make that move. Try Again."
                                      runGame state

randomGame :: GameM ()
randomGame = do 
  genRandomGame (State Black startingBoard)

genRandomGame :: State -> GameM ()
genRandomGame state@(State disc board) = do
  gameEnd state
  writeBoard board   
  case possibleMoves disc board of
    []     -> do
                liftIO $ putStrLn "#PASS#"
                let newState = (State (flipDisc disc) board)
                genRandomGame newState 
    (x:xs) -> do
                liftIO $ print disc
                loc <- genLoc state
                liftIO $ putStr "Move: "
                liftIO $ print loc
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
        liftIO $ putStrLn "Black won! White lost!"
      else 
        liftIO $ putStrLn "White won! Black lost!"
  else return ()
                                     
writeBoard :: Board -> GameM ()
writeBoard b = liftIO $ putBoard b

-- Helper functions --

gameInt :: GameM Int
gameInt = GameM $ randomRIO (0, 7) 

gameAdd :: GameM Int -> GameM Int -> GameM Int
gameAdd x y = liftM2 (+) x y 

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
passMessage disc = (discName disc) ++ " passes..."  
    
moveMessage :: Disc -> String
moveMessage disc = (discName disc) ++ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

discName :: Disc -> String
discName White = "White"
discName Black = "Black"