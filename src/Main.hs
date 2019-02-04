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

data GameState = GameState { currentDisc :: Disc, currentBoard :: Board, frames :: [GameState]  }
  deriving (Show, Eq)

newtype GameM a = GameM (StateT GameState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GameState) -- MonadState GameState is important

-- class Monad m => Logger m where
--   writeMoveMessage :: Disc -> Location -> m ()
--   writePassMessage :: Disc -> m ()
--   writeFailMessage :: Disc -> m ()
--   writePrompt :: Disc -> m ()
--   writeFinalMessage :: Final -> m ()
--   writePossibleMoves :: Disc -> Board -> m ()
--   writeBoard :: Board -> m ()

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

-- instance Logger GameM where
--   writeMoveMessage :: Disc -> Location -> GameM ()
--   writeMoveMessage disc loc = liftIO . putStrLn $ (show disc) ++ " disc placed at " ++ (show loc) ++ "."

--   writePassMessage :: Disc -> GameM ()
--   writePassMessage disc = liftIO . putStrLn . ((show disc) ++) $ " passes..."

--   writeFailMessage :: Disc -> GameM ()
--   writeFailMessage d = liftIO . putStrLn $ (show d) ++ " made an invalid move."

--   writePrompt :: Disc -> GameM ()
--   writePrompt disc = liftIO . putStrLn . ((show disc) ++) $ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

--   writeFinalMessage :: Final -> GameM ()
--   writeFinalMessage (Win disc) = liftIO . putStrLn $ (show disc) ++ " won! " ++ (show . flipDisc $ disc) ++ " lost!"
--   writeFinalMessage Tie        = liftIO . putStrLn $ "It's a tie!" 

--   writePossibleMoves :: Disc -> Board -> GameM ()
--   writePossibleMoves d = (liftIO . putStrLn . ("Possible Moves: "++) . show . possibleMoves d)

--   writeBoard :: Board -> GameM ()
--   writeBoard = liftIO . putBoard

instance Logger GameM where
  writeMoveMessage :: GameState -> Location -> GameM ()
  writeMoveMessage gs loc = liftIO . putStrLn $ moveMessage
    where
      moveMessage :: String
      moveMessage = (show $ currentDisc gs) ++ " disc placed at " ++ (show loc) ++ "."

  writePassMessage :: GameState -> GameM ()
  writePassMessage gs = liftIO . putStrLn $ passMessage 
    where 
      passMessage :: String
      passMessage = (show $ currentDisc gs) ++ " passes..."

  writeFailMessage :: GameState -> GameM ()
  writeFailMessage gs = liftIO . putStrLn $ failMessage 
    where
      failMessage :: String
      failMessage = (show $ currentDisc gs) ++ " made an invalid move."

  writePrompt :: GameState -> GameM ()
  writePrompt gs = liftIO . putStrLn $ prompt
    where
      prompt :: String
      prompt = (show $ currentDisc gs) ++ "'s move. Enter a location in the format (x,y). Ctrl + C to quit."

  writePossibleMoves :: GameState -> GameM ()
  writePossibleMoves gs = liftIO . putStrLn $ possibleMovesMsg
    where 
      possibleMovesMsg :: String 
      possibleMovesMsg = "Possible Moves: " ++ (show $ possibleMoves (currentDisc gs) (currentBoard gs))

  writeBoard :: GameState -> GameM ()
  writeBoard = liftIO . putBoard . currentBoard

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

main' :: IO ()
main' = runGameM genRandomGame

runGameM :: GameM a -> IO a
runGameM (GameM m) = evalStateT m startingState

stepGame :: (Logger m, Control m, MonadState GameState m) => m ()
stepGame = do
  gs <- get
  writeBoard gs
  case plausibleMoves gs of
    [] -> do
      writePassMessage gs
      modify changePlayer
      stepGame
    moves -> do
      writePrompt gs
      writePossibleMoves gs
      loc <- getInput
      if elem loc moves then 
        do
          writeMoveMessage gs loc
          modify $ play loc
          stepGame 
      else
        if loc == ((-1), (-1)) then
          do 
            modify rewind
            stepGame
        else 
          do
            writeFailMessage gs
            stepGame
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

--

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

-- PossibleMoves for GameState
plausibleMoves :: GameState -> [Location]
plausibleMoves gs = possibleMoves (currentDisc gs) (currentBoard gs)

-- Generate Random Game --
genRandomGame :: (Logger m, Generator m, MonadState GameState m) => m ()
genRandomGame = do
  gs <- get
  writeBoard gs   
  case plausibleMoves gs of
    [] -> do
      writePassMessage gs
      modify changePlayer
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