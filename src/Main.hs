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
