module Board where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Types

{-Start-}
putBoard :: Board -> IO ()
putBoard board = putStr step4
                   where 
                    step1 = boardToCells board
                    step2 = mapCells step1
                    step3 = cellMapToString step2
                    step4 = capBoard step3

capBoard :: String -> String 
capBoard x = "---------------------------------\n" ++ x

cellMapToString :: [(Location, Cell)] -> String
cellMapToString [] = ""
cellMapToString (((x, y), c):tail) = (if x == 7 then 
                                        (cellToString c) ++ line 
                                      else cellToString c) ++ cellMapToString tail
                                   where 
                                    line = "|\n---------------------------------\n"

cellToString :: Cell -> String
cellToString (Nothing)    = "|   " 
cellToString (Just Black) = "| B "
cellToString (Just White) = "| W " 

mapCells :: [Cell] -> [(Location, Cell)]
mapCells = (zip genKeys)

boardToCells :: Board -> [Cell]
boardToCells board = map (lookup' board) genKeys

-- For generating a list of all board locations
genKeys :: [Location]
genKeys = [(x, y) | y <- [0..7], x <- [0..7]]

lookup' :: Ord k => Map k a -> k -> Maybe a
lookup' = flip Map.lookup

-- Test boards for testing different game states

startingBoard :: Board 
startingBoard = makeBoard [((3,3), White), ((4,3), Black), ((3,4), Black), ((4,4), White)]

{-
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   | W | B |   |   |   |
---------------------------------
|   |   |   | B | W |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
-}

testBoard1 :: Board 
testBoard1 = makeBoard [((3,3), White), ((4,3), White), ((5,3), White), ((3,4), Black), ((4,4), White)]

testBoard2 :: Board 
testBoard2 = makeBoard [((3,3), White), ((4,3), White), ((3,4), Black), ((4,4), Black), ((5,4), Black), ((4,5), White), ((4,6), White)]

{-
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
|   |   |   | W | W |   |   |   |
---------------------------------
|   |   |   | B | B | B |   |   |
---------------------------------
|   |   |   |   | W |   |   |   |
---------------------------------
|   |   |   |   | W |   |   |   |
---------------------------------
|   |   |   |   |   |   |   |   |
---------------------------------
-}

testBoard3 :: Board
testBoard3 = makeBoard [((2,4), White)
                      , ((1,4), White)
                      , ((0,4), Black)
                      , ((4,4), White)
                      , ((5,4), White)
                      , ((6,4), White)
                      , ((7,4), Black)
                      , ((3,3), White)
                      , ((3,2), White)
                      , ((3,1), White)
                      , ((3,0), Black)
                      , ((3,5), White)
                      , ((3,6), White)
                      , ((3,7), Black)
                      , ((4,3), White)
                      , ((5,2), White)
                      , ((6,1), White)
                      , ((7,0), Black)
                      , ((2,5), White)
                      , ((1,6), White)
                      , ((0,7), Black)
                      , ((2,3), White)
                      , ((1,2), White)
                      , ((0,1), Black)
                      , ((4,5), White)
                      , ((5,6), White)
                      , ((6,7), Black)] 

testBoard4 :: Board 
testBoard4 = makeBoard [((3,3), White), ((4,4), Black), ((5,5), Black), ((6,6), Black)]

testBoard5 :: Board 
testBoard5 = makeBoard [((0,7), White), ((1,6), Black), ((2,5), Black), ((3,4), Black), ((4,3), Black)]

makeBoard :: [(Location, Disc)] -> Board
makeBoard = Map.fromList
{-End-}