{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main (main) where

import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy
import           Reversi.Actions
import           Reversi.Board
import           Reversi.GameTree
import           Reversi.Types
import           System.Random            (randomRIO)
import           Text.Pretty.Simple       (pPrint)
import           Text.Read                hiding (get)

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

    writeAny :: Show a => a -> GameM () -- For debugging
    writeAny = liftIO . print

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
main = runGameM genRandomGame

runGameM :: GameM a -> IO a
runGameM (GameM m) = evalStateT m startingState

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

gameEnd :: (Logger m, MonadState GameState m) => m () -- Need (MonadState GameState m) constraint
gameEnd = do
  gs <- get
  if noMoves gs then
    do
      writeBoard gs
      let final = getFinal gs
      writeFinalMessage final
  else return ()
