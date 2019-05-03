{-# LANGUAGE NamedFieldPuns #-}

module Reversi.Actions where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Reversi.Board
import Reversi.Types

flipDisc :: Disc -> Disc
flipDisc Black = White
flipDisc White = Black

placeDisc :: Location -> Disc -> Board -> Board 
placeDisc = Map.insert

checkMove :: Disc -> Location -> Board -> Bool
checkMove disc loc board = or [(checkMoveOrtho disc loc board), (checkMoveDiago disc loc board)]

checkMoveDiago :: Disc -> Location -> Board -> Bool
checkMoveDiago disc loc board = or [(isValidMoveMajor disc loc board), (isValidMoveMinor disc loc board)]

isValidMoveMinor :: Disc -> Location -> Board -> Bool 
isValidMoveMinor disc loc board = if isValidLoc loc board then answer else False 
    where
        answer            = condition1 || condition2
        preceding         = reverse $ precedingCellsMinor loc board
        following         = followingCellsMinor loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        condition1        = validateCaptured precedingCaptured
        condition2        = validateCaptured followingCaptured

isValidMoveMajor :: Disc -> Location -> Board -> Bool 
isValidMoveMajor disc loc board = if isValidLoc loc board then answer else False
    where 
        answer            = condition1 || condition2
        preceding         = reverse $ precedingCellsMajor loc board
        following         = followingCellsMajor loc board 
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        condition1        = validateCaptured precedingCaptured
        condition2        = validateCaptured followingCaptured

checkMoveOrtho :: Disc -> Location -> Board -> Bool 
checkMoveOrtho disc loc board = or [(isValidMoveRow disc loc board), (isValidMoveCol disc loc board)]

isValidMoveCol :: Disc -> Location -> Board -> Bool
isValidMoveCol disc loc board = if isValidLoc loc board then answer else False 
    where 
        answer            = condition1 || condition2
        preceding         = reverse $ precedingCellsCol loc board
        following         = followingCellsCol loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        condition1        = validateCaptured precedingCaptured
        condition2        = validateCaptured followingCaptured   

isValidMoveRow :: Disc -> Location -> Board -> Bool
isValidMoveRow disc loc board = if isValidLoc loc board then answer else False
    where 
        answer            = condition1 || condition2
        preceding         = reverse $ precedingCellsRow loc board
        following         = followingCellsRow loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        condition1        = validateCaptured precedingCaptured 
        condition2        = validateCaptured followingCaptured

validateCaptured :: ([Cell], [Cell]) -> Bool                                       
validateCaptured ([], _)   = False
validateCaptured (_, [])   = False
validateCaptured ((c:_), (t:_)) = isOppositeCell c t

isOppositeCell :: Cell -> Cell -> Bool
isOppositeCell Nothing _   = False
isOppositeCell _ Nothing   = False
isOppositeCell x y         = x /= y

followingCellsMinor :: Location -> Board -> [Cell]
followingCellsMinor location board = map (lookup' board) (followingKeysMinor location)

followingKeysMinor :: Location -> [Location]
followingKeysMinor loc@(x, y) = answer
    where
        endLoc = findEndMinor loc
        endX   = fst endLoc 
        endY   = snd endLoc 
        listX  = [(x+1)..endX]
        listY  = reverse $ [endY..(y-1)]
        answer = zip listX listY      

findEndMinor :: Location -> Location
findEndMinor loc@(x, y) = if or [x == 7, y == 0] then loc else findEndMinor (x+1, y-1)

followingCellsMajor :: Location -> Board -> [Cell]
followingCellsMajor location board = map (lookup' board) (followingKeysMajor location)

followingKeysMajor :: Location -> [Location]
followingKeysMajor loc@(x, y) = answer 
    where
        endLoc = findEndMajor loc 
        endX   = fst endLoc 
        endY   = snd endLoc 
        listX  = [(x+1)..endX]
        listY  = [(y+1)..endY]
        answer = zip listX listY

findEndMajor :: Location -> Location 
findEndMajor loc@(x, y) = if or [x == 7, y == 7] then loc else findEndMajor (x+1, y+1)

followingCellsCol :: Location -> Board -> [Cell]
followingCellsCol location board = map (lookup' board) (followingKeysCol location)
    where 
        followingKeysCol (x, y) = zip (repeat x) [(y+1)..7]                                     

followingCellsRow :: Location -> Board -> [Cell]
followingCellsRow location board = map (lookup' board) (followingKeysRow location)
    where
        followingKeysRow (x, y) = zip [(x+1)..7] (repeat y) 

precedingCellsMinor :: Location -> Board -> [Cell]
precedingCellsMinor location board = map (lookup' board) (precedingKeysMinor location)

precedingKeysMinor :: Location -> [Location]
precedingKeysMinor loc@(x, y) = answer
    where 
        startLoc = findStartMinor loc
        startX = fst startLoc
        startY = snd startLoc
        listX = [startX..(x-1)]
        listY = reverse $ [(y+1)..startY]
        answer = zip listX listY

findStartMinor :: Location -> Location
findStartMinor loc@(x, y) = if or [x == 0, y == 7] then loc else findStartMinor (x-1, y+1) 

precedingCellsMajor :: Location -> Board -> [Cell]
precedingCellsMajor location board = map (lookup' board) (precedingKeysMajor location)
                                --where
precedingKeysMajor :: Location -> [Location]
precedingKeysMajor loc@(x, y) = answer
    where
        startLoc  = findStartMajor loc 
        startX    = fst startLoc
        startY    = snd startLoc 
        listX     = [startX..(x-1)]
        listY     = [startY..(y-1)]
        answer    = zip listX listY 

findStartMajor :: Location -> Location
findStartMajor loc@(x, y) = if or [x == 0, y == 0] then loc else findStartMajor $ backMajor loc

backMajor :: Location -> Location
backMajor (x, y) = (x - 1, y - 1)

sameLoc :: Location -> Location -> Bool
sameLoc loc1 loc2 = loc1 == loc2

isEdge :: Location -> Bool
isEdge (x, y) = or [(x == 0 || y == 0), (x == 7 || y == 7)] 

precedingCellsCol :: Location -> Board -> [Cell] 
precedingCellsCol location board = map (lookup' board) (precedingKeysCol location)
    where
        precedingKeysCol (x, y) = zip (repeat x) [0..(y-1)] 
                        
precedingCellsRow :: Location -> Board -> [Cell]
precedingCellsRow location board = map (lookup' board) (precedingKeysRow location)
    where
        precedingKeysRow (x, y) = zip [0..(x-1)] (repeat y)

isValidLoc :: Location -> Board -> Bool
isValidLoc loc board = and [(isOpenLoc loc board), (isInside loc)]

isInside :: Location -> Bool
isInside (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

isOpenLoc :: Location -> Board -> Bool
isOpenLoc loc board = isEmptyCell $ getCell loc board

isEmptyCell :: Cell -> Bool
isEmptyCell Nothing = True
isEmptyCell _       = False

getCell :: Location -> Board -> Cell
getCell = Map.lookup 

makeMoveDiago :: Disc -> Location -> Board -> Board
makeMoveDiago disc loc = (makeMoveMinor disc loc) . (makeMoveMajor disc loc) 

makeMoveMinor :: Disc -> Location -> Board -> Board
makeMoveMinor disc loc board = if isValidLoc loc board then answer else board
    where 
        preceding         = reverse $ precedingCellsMinor loc board
        following         = followingCellsMinor loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following 
        precedingFlipped  = reverse $ flipCaptured precedingCaptured
        followingFlipped  = flipCaptured followingCaptured
        newCells1         = precedingFlipped ++ [Nothing] ++ followingFlipped 
        newCellMap2       = zip (getMinorKeys loc) newCells1
        newCellMap3       = filter (\x -> not ((snd x) == Nothing)) newCellMap2
        newMinorKeys      = map fst newCellMap3
        newCells2         = map snd newCellMap3
        newDiscs          = map fromJust newCells2
        newMinor          = makeBoard (zip newMinorKeys newDiscs)
        answer            = Map.union newMinor board 

makeMoveMajor :: Disc -> Location -> Board -> Board
makeMoveMajor disc loc board = if isValidLoc loc board then answer else board
    where
        preceding         = reverse $ precedingCellsMajor loc board
        following         = followingCellsMajor loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        precedingFlipped  = reverse $ flipCaptured precedingCaptured
        followingFlipped  = flipCaptured followingCaptured
        newCells1         = precedingFlipped ++ [Nothing] ++ followingFlipped
        newCellMap2       = zip (getMajorKeys loc) newCells1 
        newCellMap3       = filter (\x -> not ((snd x) == Nothing)) newCellMap2
        newMajorKeys      = map fst newCellMap3
        newCells2         = map snd newCellMap3
        newDiscs          = map fromJust newCells2 
        newMajor          = makeBoard (zip newMajorKeys newDiscs)
        answer            = Map.union newMajor board 

makeMoveOrtho :: Disc -> Location -> Board -> Board
makeMoveOrtho disc loc = (makeMoveCol disc loc) . (makeMoveRow disc loc) 

makeMoveCol :: Disc -> Location -> Board -> Board 
makeMoveCol disc loc board = if isValidLoc loc board then answer else board
    where
        preceding = reverse $ precedingCellsCol loc board
        following = followingCellsCol loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        precedingFlipped  = reverse $ flipCaptured precedingCaptured
        followingFlipped  = flipCaptured followingCaptured
        newCells1         = precedingFlipped ++ [Nothing] ++ followingFlipped
        newCellMap2       = zip (getColKeys loc) newCells1
        newCellMap3       = filter (\x -> not ((snd x) == Nothing)) newCellMap2
        newColKeys        = map fst newCellMap3
        newCells2         = map snd newCellMap3
        newDiscs          = map fromJust newCells2
        newCol            = makeBoard (zip newColKeys newDiscs)
        answer            = Map.union newCol board

makeMoveRow :: Disc -> Location -> Board -> Board
makeMoveRow disc loc board = if isValidLoc loc board then answer else board
    where 
        preceding         = reverse $ precedingCellsRow loc board 
        following         = followingCellsRow loc board
        precedingCaptured = getCaptured (Just disc) preceding
        followingCaptured = getCaptured (Just disc) following
        precedingFlipped  = reverse $ flipCaptured precedingCaptured
        followingFlipped  = flipCaptured followingCaptured
        newCells1         = precedingFlipped ++ [Nothing] ++ followingFlipped
        newCellMap2       = zip (getRowKeys loc) newCells1 
        newCellMap3       = filter (\x -> not ((snd x) == Nothing)) newCellMap2 -- remove Nothing elements
        newRowKeys        = map fst newCellMap3      -- Get the keys of the Just _ Cells 
        newCells2         = map snd newCellMap3      -- Get Just _ Cells
        newDiscs          = map fromJust newCells2   -- take every cell and make it a disc (unbox from Just "context") -- fromJust can throw an err :(
        newRow            = makeBoard (zip newRowKeys newDiscs) -- make newRow from newRowKeys and newDiscs zipped together
        answer            = Map.union newRow board  -- insert newRow into board using union

getMinorKeys :: Location -> [Location] 
getMinorKeys loc = zip [ a | a <- [startX..endX]] $ reverse $ [ b | b <- [endY..startY]]
    where
        startX = fst $ findStartMinor loc
        endX   = fst $ findEndMinor loc
        startY = snd $ findStartMinor loc
        endY   = snd $ findEndMinor loc

getMajorKeys :: Location -> [Location]
getMajorKeys loc = zip [ a | a <- [startX..endX]] [ b | b <- [startY..endY]]
    where 
        startX = fst $ findStartMajor loc
        endX   = fst $ findEndMajor loc 
        startY = snd $ findStartMajor loc
        endY   = snd $ findEndMajor loc                                   

getColKeys :: Location -> [Location]
getColKeys (x, _) = [(x, b) | b <- [0..7]]                                      

getRowKeys :: Location -> [Location]
getRowKeys (_, y) = [(a, y) | a <- [0..7]]

flipCaptured :: ([Cell], [Cell]) -> [Cell]
flipCaptured ([], [])                       = []
flipCaptured ([], end)                      = end
flipCaptured (captured, [])                 = captured
flipCaptured (captured@(c:_), end@(t:_)) = if isOppositeCell c t then (map flipCell captured) ++ end else captured ++ end

flipCell :: Cell -> Cell
flipCell Nothing      = Nothing
flipCell (Just Black) = Just White
flipCell (Just White) = Just Black

getCaptured :: Cell -> [Cell] -> ([Cell], [Cell])
getCaptured measure cells = ((takeWhile (isOppositeCell measure) cells), (dropWhile (isOppositeCell measure) cells)) 

makeMove :: Disc -> Location -> Board -> Board
makeMove disc loc = ((placeDisc loc disc) . (makeMoveDiago disc loc) . (makeMoveOrtho disc loc))

-- For modifying GameState
play :: Location -> GameState -> GameState
play loc gs@GameState{ getDisc = currDisc, getBoard = currBoard, getMove, getFrames = currFrames } = -- Maybe change curr to old
  GameState{ getDisc = (flipDisc currDisc), getBoard = makeMove currDisc loc currBoard, getMove = Move loc, getFrames = gs:currFrames }

pass :: GameState -> GameState
pass gs@GameState{ getDisc = currDisc, getBoard = currBoard, getMove, getFrames = currFrames } = 
  GameState{ getDisc = (flipDisc currDisc), getBoard = currBoard, getMove = Pass, getFrames = gs:currFrames } -- Probably a better way to construct this data

-- possibleMoves for GameState
plausibleMoves :: GameState -> [Location]
plausibleMoves gs = possibleMoves (getDisc gs) (getBoard gs)

-- Functions for creating a list of all possible moves for a given color of Disc
possibleMoves :: Disc -> Board -> [Location]
possibleMoves disc board = answer 
  where
    sieve  = (flip (checkMove disc)) board
    answer = filter sieve genKeys

rewind :: GameState -> GameState
rewind (GameState disc board m []) = GameState disc board m [] 
rewind (GameState _ _ _ (f:fs))    = GameState (getDisc f) (getBoard f) (getMove f) fs

noMoves :: GameState -> Bool
noMoves (GameState disc board _ _) = ((length $ possibleMoves disc board) == 0) && ((length $ possibleMoves (flipDisc disc) board) == 0)

getFinal :: GameState -> Final
getFinal (GameState d b _ _)
        | countDiscs d b == countDiscs (flipDisc d) b = Tie
        | otherwise = if countDiscs d b > countDiscs (flipDisc d) b then Win d else Win (flipDisc d) 
    
countDiscs :: Disc -> Board -> Int
countDiscs d = Map.size . (Map.filter ((==) d))