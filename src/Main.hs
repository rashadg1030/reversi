data Disc = B | W 
  deriving (Show, Eq)

type Cell = Maybe Disc

type Row = [Cell]

type Board = [Row]

type LocX = Int 
type LocY = Int

type Location = (LocX, LocY)

startingBoard :: Board
startingBoard = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Just W, Just B, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Just B, Just W, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                 [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

displayBoard :: Board -> IO ()
displayBoard = putStr . (++) "\n-----------------\n" . boardString 

-- Helper functions for displayBoard
boardString :: Board -> String
boardString [] = "ERR: INVALID BOARD"
boardString [x] = rowString x
boardString (x:xs) = rowString x ++ boardString xs 

rowString :: Row -> String
rowString [] = "ERR: INVALID ROW"
rowString [x] = cellString x ++ "|\n-----------------\n"
rowString (x:xs) = cellString x ++ rowString xs

cellString :: Cell -> String
cellString (Nothing) = "| "
cellString (Just B) = "|B"
cellString (Just W) = "|W"

-- For placing a disc within a row on the board
placeRow :: Maybe Disc -> LocX -> Row -> Row
placeRow disc loc = (replaceCell disc loc) . keyRow

replaceCell :: Maybe Disc -> LocX -> [(LocX, Cell)] -> Row
replaceCell _ _ [] = []
replaceCell disc loc row = [if x == loc then disc else c | (x, c) <- row ] 

-- Gives each cell in a row a LocX
keyRow :: Row -> [(LocX, Cell)]
keyRow [] = []
keyRow row = zip [0..] row

hasMoves :: Disc -> Board -> Bool
hasMoves x = undefined

flip :: Board -> Board
flip = undefined

