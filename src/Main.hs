module Main where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Functor

main :: IO ()
main = undefined

-- Data

data Disc = Black | White
    deriving (Show, Eq)
  
type Cell = Maybe Disc

type Location = (Int, Int)

type Board = Map Location Disc

-- Functions

-- For displaying boards
putBoard :: Board -> IO ()
putBoard board = putStr step4
                   where 
                    step1 = boardToCells board
                    step2 = mapCells step1
                    step3 = cellMapToString step2
                    step4 = capBoard step3

capBoard :: String -> String 
capBoard x = "---------------------------------\n" ++ x

cellMapToString :: [((Int, Int), Cell)] -> String
cellMapToString [] = ""
cellMapToString (((x, y), c):tail) = (if x == 7 then (cellToString c) ++ "|\n---------------------------------\n" else cellToString c) ++ cellMapToString tail

cellToString :: Cell -> String
cellToString (Nothing)    = "|   " 
cellToString (Just Black) = "| B "
cellToString (Just White) = "| W " 

mapCells :: [Cell] -> [((Int, Int), Cell)]
mapCells = (zip genKeys)

boardToCells :: Board -> [Cell]
boardToCells board = map (lookup' board) genKeys

lookup' :: Ord k => Map k a -> k -> Maybe a
lookup' = flip Map.lookup

startingBoard :: Board 
startingBoard = makeBoard [((3,3), White), ((4,3), Black), ((3,4), Black), ((4,4), White), ((5,3), White), ((7,4), Black)]

makeBoard :: [((Int, Int), Disc)] -> Board
makeBoard = Map.fromList

genKeys :: [(Int, Int)]
genKeys = [(x, y) | y <- [0..7], x <- [0..7]]

-- Functions for creating a list of all possible moves for a given color of disc
validMoves :: Disc -> Board -> [Location]
validMoves disc board = undefined

isValidMove :: Disc -> Location -> Board -> Bool
isValidMove disc loc@(x, y) board
  | (isInside loc) && (isEmptyCell $ getCell loc board) = answer                                  
  | otherwise                                           = False
                                                        where 
                                                          answer = undefined
isOpenLoc :: Location -> Board -> Bool
isOpenLoc loc board = isEmptyCell $ getCell loc board


getCell :: Location -> Board -> Cell
getCell = Map.lookup 
                                            
isEmptyCell :: Cell -> Bool
isEmptyCell Nothing = True
isEmptyCell _       = False

getRow :: Int -> Board -> Board
getRow locY board = Map.filterWithKey (\(x, y) _ -> y == locY) board

getColumn :: Int -> Board -> Board 
getColumn locX board = Map.filterWithKey (\(x, y) _ -> x == locX) board

isInside :: Location -> Bool
isInside (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

{--
-- Make play vertically and horizontally
playXY :: Cell -> Location -> Board -> Board
playXY Empty _ board = board
playXY _ _ [] = []
playXY disc loc@(x, y) board = if isEmpty then ((changeCell disc loc) . (playCol disc loc) . (changeCell Empty loc) . (playRow disc loc)) board else board 
                            where
                              isEmpty = checkLocX x $ head $ snd <$> (dropWhile (\(key, _) -> (key < y)) $ mapList board)

-- playRow works fine 
playRow :: Cell -> Location -> Board -> Board
playRow Empty _ board = board 
playRow disc (x, y) board = before ++ newRow ++ after
                where
                  before = snd <$> (takeWhile (\(key, _) -> (key < y)) $ mapList board)
                  after  = snd <$> (dropWhile (\(key, _) -> (key <= y)) $ mapList board)
                  newRow = (playHorizontal disc x) <$> (snd <$> (filter (\(key, _) -> (key == y)) $ mapList board))

playCol :: Cell -> Location -> Board -> Board  
playCol disc (x, y) = transpose . (playRow disc (y, x)) . transpose

-- Helper function for playHorizontal that checks if LocX is an Empty cell
checkLocX :: LocX -> Row -> Bool
checkLocX locX row = (length emptyCellAndMatch) > 0
                  where 
                    cellMap = mapList row
                    emptyCellMap = filter isEmptyCell cellMap
                    emptyCellAndMatch = filter (\(fst, snd) -> fst == locX) emptyCellMap 

-- Takes a locX and board and returns a column for processing
{--
getCol :: LocX -> Board -> Board 
getCol locX board = playHorizontal <$> snd <$> filter f columnMap
                     where 
                      columnMap = mapList $ transpose board
                      f = (\(key, _) -> (key == locX))
--}

locFits :: Location -> Bool
locFits (locX, locY) = validLocX && validLocY 
                          where 
                            validLocX = (locX >= 0) || (locX <= 7)
                            validLocY = (locY >= 0) || (locY <= 7)

-- Might create type Column = [Cell]??
-- Might need Maybe
columnsToRows :: Board -> Board
columnsToRows = transpose 

{--
columnToRow locX []     = []
columnToRow locX board  = undefined --take 1 $ dropWhile (\(key, column) -> not (key == locX)) columnMap
                      where
                        columns = transpose board
                        columnMap = mapList columns
--}
-- For changing a cell on the board.
changeCell :: Cell -> Location -> Board -> Board
changeCell cell (locX, locY) board = [if key == locY then (changeCellRow cell locX row) else row | (key, row) <- (mapList board)]

-- For changing a cell in a row. 
changeCellRow :: Cell -> LocX -> Row -> Row
changeCellRow _ _ []           = []
changeCellRow newCell locX row = [if key == locX then newCell else cell | (key, cell) <- (mapList row)] 

-- Maps each item in the list to an Int.
mapList :: [a] -> [(Int, a)]
mapList = (zip [0..])

-- Check if board cell is empty
isEmptyCell :: (LocX, Cell) -> Bool
isEmptyCell (_, Empty) = True
isEmptyCell _          = False

isBlackDisc :: Cell -> Bool
isBlackDisc Black = True
isBlackDisc _     = False

isWhiteDisc :: Cell -> Bool
isWhiteDisc White = True
isWhiteDisc _     = False

playHorizontal :: Cell -> LocX -> Row -> Row
playHorizontal Empty _ row   = row
playHorizontal disc locX row = if canPlay then newLeft ++ [disc] ++ newRight else row
                              where  
                                left          =  reverse $ fst $ shaveRow locX row
                                right         =  snd $ shaveRow locX row
                                leftDivided   =  divideRow disc left
                                rightDivided  =  divideRow disc right
                                newLeft       = reverse $ flipCaptured leftDivided
                                newRight      = flipCaptured rightDivided
                                canPlay       = (checkRowPair leftDivided) || (checkRowPair rightDivided)

{--
  Thee following functions are helper functions for checking a valid play on the board.
--}

{--
This is only work in one direction. Changing it up so that it creates a pair rows. One row consisting of cells 
before the play cell, and one consisting of cells coming after it. 
  shaveRow :: LocX -> Row -> Row
  shaveRow locX = drop $ locX + 1  
--}

{--
playHorizontal :: Cell -> LocX -> Row -> Row
playHorizontal Empty _ row   = row
playHorizontal disc locX row = if checkLocX locX row then (if canPlay then newLeft ++ [disc] ++ newRight else row) else row
                              where  
                                left          =  reverse $ fst $ shaveRow locX row
                                right         =  snd $ shaveRow locX row
                                leftDivided   =  divideRow disc left
                                rightDivided  =  divideRow disc right
                                newLeft       = reverse $ flipCaptured leftDivided
                                newRight      = flipCaptured rightDivided
                                canPlay       = (checkRowPair leftDivided) || (checkRowPair rightDivided)
--}
-- Not really shaving, but I don't have a better name. This splits the row into two rows. First step.
-- Reverse the row that is on the left of the play cell so that it can be checked properly.
shaveRow :: LocX -> Row -> (Row, Row)
shaveRow locX row = ((take locX row), (drop (locX + 1) row))  

-- Then, divide the shaved row into two rows according to color of disc that is being played.
divideRow :: Cell -> Row -> (Row, Row)
divideRow measure row = ((takeWhile (isOppositeCell measure) row), (dropWhile (isOppositeCell measure) row))

-- do something after checkRowPair to flip cells in row pair. Last step.
flipCaptured :: (Row, Row) -> Row 
flipCaptured ([], [])         = []
flipCaptured ([], tail)       = tail
flipCaptured (captured, [])   = captured
flipCaptured (captured@(c:cs), tail@(t:ts)) = if isOppositeCell c t then (flipRow captured) ++ tail else captured ++ tail 
--flipCaptured (captured, tail) = (flipRow captured) ++ tail 

-- Flip all cells in a row
flipRow :: Row -> Row
flipRow = (map flipCell)
 
-- FLips a disc to the opposite color
flipCell :: Cell -> Cell
flipCell Empty = Empty
flipCell Black = White
flipCell White = Black

-- Then based on the pair of rows, decide if play is valid or not.
-- Might not need?
checkRowPair :: (Row, Row) -> Bool
checkRowPair ([], [])         = False
checkRowPair ([], tail)       = False
checkRowPair (captured, [])   = False
checkRowPair ((c:cs), (t:ts)) = isOppositeCell c t

isOppositeCell :: Cell -> Cell -> Bool
isOppositeCell Black White = True
isOppositeCell White Black = True
isOppositeCell _ _         = False

isSameCell :: Cell -> Cell -> Bool 
isSameCell = (==) 

isSameCellMap :: Cell -> Row -> [Bool]
isSameCellMap measure = map (isSameCell measure) 

-- Will check to the right of the disk for a valid move
{--
checkRight :: Cell -> LocX -> Row -> Cell
checkRight disc locX row = rightOfLocX
  where rightOfLocX = drop locX row
--}
--}

