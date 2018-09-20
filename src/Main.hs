data Disc = B | W 
  deriving (Show, Eq)

type Cell = Maybe Disc

type Row = [Cell]

type Board = [Row]

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


placeDisc :: Disc -> Board -> (Char, Int) -> Board
placeDisc x y (c, i) = undefined

hasMoves :: Disc -> Board -> Bool
hasMoves x = undefined

flip :: Board -> Board
flip = undefined

