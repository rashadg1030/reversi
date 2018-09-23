module Main where

main :: IO ()
main = undefined

data Cell = Black | White | Empty
  deriving (Show, Eq)

-- data Cell = B | W 
--  deriving (Show, Eq)

-- type Cell = Maybe Disc

type Row = [Cell]

type Board = [Row]

type LocX = Int 
type LocY = Int

type Location = (LocX, LocY)

startingBoard :: Board
startingBoard = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, White, Black, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Black, White, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]

displayBoard :: Board -> IO ()
displayBoard = putStr . (++) "\n---------------------------------\n" . boardString 

-- Creates a string representation of a board.
boardString :: Board -> String
boardString []     = "ERR: INVALID BOARD"
boardString [x]    = rowString x
boardString (x:xs) = rowString x ++ boardString xs 

-- Creates a string representation of a row.
rowString :: Row -> String
rowString []     = "ERR: INVALID ROW"
rowString [x]    = cellString x ++ "|\n---------------------------------\n"
rowString (x:xs) = cellString x ++ rowString xs

-- Creates a string representation of a cell.
cellString :: Cell -> String
cellString Empty = "|   "
cellString Black = "| B "
cellString White = "| W "

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
isEmptyCell :: Cell -> Bool
isEmptyCell Empty = True
isEmptyCell _     = False

isBlackDisc :: Cell -> Bool
isBlackDisc Black = True
isBlackDisc _     = False

isWhiteDisc :: Cell -> Bool
isWhiteDisc White = True
isWhiteDisc _     = False

{--
  Thee following functions are helper functions for checking a valid play on the board.
--}

-- First, drop the first LocX + 1 elements of the list
{--
This is only work in one direction. Changing it up so that it creates a pair rows. One row consisting of cells 
before the play cell, and one consisting of cells coming after it. 
  shaveRow :: LocX -> Row -> Row
  shaveRow locX = drop $ locX + 1  
--}

-- Not really shaving, but I don't have a better name. This splits the row into two rows. First step.
-- Reverse the row that is on the left of the play cell so that it can be checked properly.
shaveRow :: LocX -> Row -> (Row, Row)
shaveRow locX row = (reverse (drop (locX + 1) row), take (locX - 1) row)  



-- Then, divide the shaved row into two rows according to color of disc that is being played.
divideRow :: Cell -> Row -> (Row, Row)
divideRow measure row = ((takeWhile (isOppositeCell measure) row), (dropWhile (isOppositeCell measure) row))

-- Then based on the pair of rows, decide if play is valid or not.
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


-- Will determine if given location is a possible move on the board for the given color of disc.
canPlace :: Cell -> Location -> Board -> Bool
canPlace = undefined

-- Will check to the right of the disk for a valid move
{--
checkRight :: Cell -> LocX -> Row -> Cell
checkRight disc locX row = rightOfLocX
  where rightOfLocX = drop locX row
--}


-- Will determine if given locX is a possible move within the row for the given color of disc.
checkHorizontal :: Cell -> LocX -> Row -> Bool
checkHorizontal disc locX row = undefined

-- Check sides of given LocX for 
checkSides :: Cell -> LocX -> Row -> Bool
checkSides disc locX row = undefined

-- Flip all cells in a row
flipRow :: Row -> Row
flipRow = (map flipCell)
 
-- FLips a disc to the opposite color
flipCell :: Cell -> Cell
flipCell Empty = Empty
flipCell Black = White
flipCell White = Black

