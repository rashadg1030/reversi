{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Heuristic where

import Actions
import Board
import Types
import qualified Data.Map as Map 

data Heat = Cold | Warm | Hot
    deriving (Show)

type HeatMap = Map.Map Location Heat

-- Risk Regions --
allLocs :: [Location]
allLocs = cornerLocs ++ cornerAdjLocs ++ edgeLocs ++ edgeAdjLocs ++ centerLocs

cornerLocs :: [Location] -- corner locations
cornerLocs = [(0,0), (7,0), (0,7), (7,7)]

cornerAdjLocs :: [Location] -- Locations adjacent to corner locations
cornerAdjLocs = [(0,1), (1,1), (1,0), (6,0), (6,1), (7,1), (0,6), (1,6), (1,7), (6,7), (6,6), (7,6)]

edgeLocs :: [Location] -- Locations at the edge
edgeLocs = [(0,2), (0,3), (0,4), (0,5), (2,0), (3,0), (4,0), (5,0), (7,2), (7,3), (7,4), (7,5), (2,7), (3,7), (4,7), (5,7)]

edgeAdjLocs :: [Location] -- Locations adj to edge locations
edgeAdjLocs = [(1,2), (1,3), (1,4), (1,5), (2,1), (3,1), (4,1), (5,1), (6,2), (6,3), (6,4), (6,5), (2,6), (3,6), (4,6), (5,6)]

centerLocs :: [Location] -- Locations in the center
centerLocs = [(2,2), (3,2), (4,2), (5,2), (2,3), (3,3), (4,3), (5,3), (2,4), (3,4), (4,4), (5,4), (2,5), (3,5), (4,5), (5,5)]

makeHeatMap :: Heat -> [Location] -> Map.Map Location Heat
makeHeatMap _ []      = Map.empty
makeHeatMap heat locs = Map.fromList $ swap <$> (heat,) <$> locs  
    where
        swap :: (a,b) -> (b,a)
        swap (x,y) = (y,x)        

heatMap :: HeatMap
heatMap = hotMap `Map.union` (warmMap `Map.union` coldMap)   

hotMap :: HeatMap
hotMap = makeHeatMap Hot $ cornerLocs ++ edgeLocs

warmMap :: HeatMap
warmMap = makeHeatMap Warm $ centerLocs

coldMap :: HeatMap
coldMap = makeHeatMap Cold $ cornerAdjLocs ++ edgeAdjLocs

-- Tests --
test1 :: Int
test1 = evalBoard Black startingBoard -- Should be zero

test2 :: Int
test2 = evalBoard White startingBoard -- Should be zero

test3 :: Int
test3 = evalBoard Black testBoard1 -- Should -3

test4 :: Int
test4 = evalBoard White testBoard1 -- Should +3

test5 :: Int
test5 = evalBoard Black testBoard2 -- Should 2

test6 :: Int
test6 = evalBoard White testBoard2 -- Should -2

test7 :: Int 
test7 = evalBoard Black testBoard3 -- Should 13

test8 :: Int 
test8 = evalBoard White testBoard4 -- It's 1 but I think this should be higher. Actually this maybe fine.

test9 :: Int  
test9 = evalBoard White testBoard6 -- Should be -20

evalBoard :: Disc -> Board -> Int -- The disc being passed in is the maximizing player
evalBoard d b
    | length (possibleMoves d b) == 0 = (-1000) -- In case there is no moves. Must pass. The maximizing player tries to not to do this. This may not be necessary 
    | otherwise = score d $ heatDiscMap heatMap b

score :: Disc -> Map.Map Location (Heat,Disc) -> Int
score d hdm = Map.foldr (+) 0 scoreMap
    where
        scoreMap = Map.map (heatDiscToInt d) hdm 

heatDiscToInt :: Disc -> (Heat, Disc) -> Int --The first arguement is the maximizing player
heatDiscToInt d hd
    | snd hd == d = case fst hd of
                        Hot  -> 2
                        Warm -> 1
                        Cold -> (-2)
    | otherwise   = case fst hd of
                        Hot  -> (-2)
                        Warm -> (-1)
                        Cold -> 2  

-- mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
-- mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b

heatDiscMap :: HeatMap -> Board -> Map.Map Location (Heat,Disc)
heatDiscMap hm b = Map.mapMaybeWithKey (someFunc hm) b
    where
        someFunc :: HeatMap -> Location -> Disc -> Maybe (Heat,Disc)
        someFunc hm' l d = case Map.lookup l hm' of
            Nothing -> Nothing
            Just h  -> Just (h,d)  
            
{-          Heat Map
---------------------------------
| H | C | H | H | H | H | C | H |
---------------------------------
| C | C | C | C | C | C | C | C |
---------------------------------
| H | C | W | W | W | W | C | H |
---------------------------------
| H | C | W | W | W | W | C | H |
---------------------------------
| H | C | W | W | W | W | C | H |
---------------------------------
| H | C | W | W | W | W | C | H |
---------------------------------
| C | C | C | C | C | C | C | C |
---------------------------------
| H | C | H | H | H | H | C | H |
---------------------------------
-}

-- The disc score is a basic score calculated as the difference between the played Disc and the opponent.
-- The heat score is a bit more complicated. It is calculated from three sub scores.
-- The hot, warm, and cold scores. The hot score is calulcated by determining how many 
-- discs occupy any zone marked as 'hot'. That number of discs is multiplied by 2 (subject to change).
-- The same method is used determining the score for other heat zones, but the multiplier is different. 
-- x1 multiplier for warm zones (subject to change) and x(-2) for cold zones (subject to change).
-- I guess there is two ways of doing this. I could calculate the score of each heat zone and add the three scores together.
-- Or I can just get the final heat score in one fell swoop by using filters and some function composition. 
-- Finally, the discScore and heatScore are added together to determine the score of the board state.


-- But wait...This is even better. Get rid of disc score entirely and really on the heatScore alone. But this time
-- also calculate the heat score based on opponent positions. 
-- So, if a square is occupied by A.I.: hot is x2, warm is x1, cold is x(-2)
-- If the square is occupied by the opp: hot is x(-2), warm is x1, cold is x(2)  