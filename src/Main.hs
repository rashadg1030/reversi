{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-}  

module Main where
  
import Control.Monad.IO.Class
import System.Random (randomRIO)
import Text.Read hiding (get)
import Actions
import Board
import Types 
import GameTree
import Control.Monad.State.Lazy
import Text.Pretty.Simple (pPrint)

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

class Monad m => Control m where
  getInput :: m Location

class Monad m => Generator m where 
  randomLoc :: m Location

instance Logger GameM where
  writeMoveMessage :: GameState -> Location -> GameM ()
  writeMoveMessage gs loc = liftIO . putStrLn $ moveMessage
    where
      moveMessage :: String
      moveMessage = (show $ getDisc gs) ++ " disc placed at " ++ (show loc) ++ "."

  writePassMessage :: GameState -> GameM ()
  writePassMessage gs = liftIO . putStrLn $ passMessage 
    where 
      passMessage :: String
      passMessage = (show $ getDisc gs) ++ " passes..."

  writeFailMessage :: GameState -> GameM ()
  writeFailMessage gs = liftIO . putStrLn $ failMessage 
    where
      failMessage :: String
      failMessage = (show $ getDisc gs) ++ " made an invalid move."

  writePrompt :: GameState -> GameM ()
  writePrompt gs = liftIO . putStrLn $ prompt
    where
      prompt :: String
      prompt = (show $ getDisc gs) ++ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

  writePossibleMoves :: GameState -> GameM ()
  writePossibleMoves gs = liftIO . putStrLn $ possibleMovesMsg
    where 
      possibleMovesMsg :: String 
      possibleMovesMsg = "Possible Moves: " ++ (show $ possibleMoves (getDisc gs) (getBoard gs))

  writeBoard :: GameState -> GameM ()
  writeBoard = liftIO . putBoard . getBoard

  writeFinalMessage :: Final -> GameM ()
  writeFinalMessage (Win disc) = liftIO . putStrLn $ (show disc) ++ " won! " ++ (show . flipDisc $ disc) ++ " lost!"
  writeFinalMessage Tie        = liftIO . putStrLn $ "It's a tie!" 

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

randomGame :: IO ()
randomGame = runGameM genRandomGame

runGameM :: GameM a -> IO a
runGameM (GameM m) = evalStateT m startingState

stepGame :: (Logger m, Control m, MonadState GameState m) => m ()
stepGame = do
  gs <- get
  writeBoard gs
  case plausibleMoves gs of
    [] -> do
      writePassMessage gs
      modify pass
    moves -> do
      writePrompt gs
      writePossibleMoves gs
      loc <- getInput
      if elem loc moves then 
        do
          writeMoveMessage gs loc
          modify $ play loc 
      else
        if loc == ((-1), (-1)) then
          do 
            modify rewind
        else 
          do
            writeFailMessage gs
  if noMoves gs then gameEnd else stepGame

gameEnd :: (Logger m, MonadState GameState m) => m () -- Need (MonadState GameState m) constraint
gameEnd = do
  gs <- get 
  if noMoves gs then
    do 
      writeBoard gs
      let final = getFinal gs
      writeFinalMessage final
  else return ()

-- Generate Random Game --
-- Throws error !!!!!
genRandomGame :: (Logger m, Generator m, MonadState GameState m) => m ()
genRandomGame = do
  gs <- get
  writeBoard gs   
  case plausibleMoves gs of
    [] -> do
      writePassMessage gs
      modify pass
    _  -> do
      loc <- genLoc gs
      writeMoveMessage gs loc
      modify $ play loc
  if noMoves gs then gameEnd else genRandomGame
                   
genLoc :: Generator m => GameState -> m (Int, Int)
genLoc gs = do
  let possible = plausibleMoves gs
  loc <- randomLoc
  if elem loc possible then return loc else genLoc gs  

test0 :: IO ()
test0 = pPrint $ genGameTree 0 (Node startingState [])

test1 :: IO ()
test1 = pPrint $ genGameTree 1 (Node startingState []) 

test2 :: IO ()
test2 = pPrint $ genGameTree 2 (Node startingState [])

test3 :: IO ()
test3 = pPrint $ genGameTree 3 (Node startingState []) 

-- aiGame :: (Logger m, Control m, MonadState GameState m) => m ()
-- aiGame = do
--   gs <- get
--   writeBoard gs
--   if getDisc gs == Black then
--     case plausibleMoves gs of
--       [] -> do
--         writePassMessage gs
--         modify pass
--       moves -> do
--         writePrompt gs
--         writePossibleMoves gs
--         loc <- getInput
--         if elem loc moves then 
--           do
--             writeMoveMessage gs loc
--             modify $ play loc 
--         else
--           if loc == ((-1), (-1)) then
--             do 
--               modify rewind
--           else 
--             do
--               writeFailMessage gs
--   else
--     case plausibleMoves gs of
--       [] -> do
--         writePassMessage gs
--         modify pass
--       moves -> do
--         writePrompt gs
--         writePossibleMoves gs
--         loc <- getAIMove White 3 (genGameTree 3 (toSeed gs))
--         if elem loc moves then 
--           do
--             writeMoveMessage gs loc
--             modify $ play loc 
--         else
--           do
--             writeFailMessage gs

--   if noMoves gs then gameEnd else aiGame
