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
placeDisc :: Maybe Disc -> Location -> Board -> Board
placeDisc disc (locX, locY) board = [if key == locY then (placeDiscX disc locX row) else row | (key, row) <- (mapList board)]

-- Places a disc in a row. Depends on the LocX. 
placeDiscX :: Maybe Disc -> LocX -> Row -> Row
placeDiscX _ _ [] = []
placeDiscX disc locX row = [if key == locX then disc else cell | (key, cell) <- (mapList row)] 

-- Maps each item in the list to an Int
mapList :: [a] -> [(Int, a)]
mapList = (zip [0..])

hasMoves :: Disc -> Board -> Bool
hasMoves x = undefined

flip :: Board -> Board
flip = undefined

