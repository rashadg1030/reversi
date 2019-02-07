{-# LANGUAGE NamedFieldPuns #-}

module Heuristic where

import Actions
import Board
import Types
import qualified Data.Map as Map 

data Heat = Cold | Warm | Hot

-- Risk Regions --
cornerLocs :: [Location] -- corner locations
cornerLocs = [(0,0), (7,0), (0,7), (7,7)]

cornerAdjLocs :: [Location] -- Locations adjacent to corner locations
cornerAdjLocs = [(0,1), (1,1), (1,0), (6,0), (6,1), (7,1), (0, 6), (1, 6), (1,7), (6,7), (6,6), (7,6)]

edgeLocs :: [Location] -- Locations at the edge
edgeLocs = [(0,2), (0,3), (0,4), (0,5), (2,0), (3,0), (4,0), (5,0), (7,2), (7,3), (7,4), (7,5), (2,7), (3,7), (4,7), (5,7)]

edgeAdjLocs :: [Location] -- Locations adj to edge locations
edgeAdjLocs = [(1,2), (1,3), (1,4), (1,5), (2,1), (3,1), (4,1), (5,1), (6,2), (6,3), (6,4), (6,5), (2,6), (3,6), (4,6), (5,6)]

centerLocs :: [Location] -- Locations in the center
centerLocs = [(2,2), (3,2), (4,2), (5,2), (2,3), (3,3), (4,3), (5,3), (2,4), (3,4), (4,4), (5,4), (2,5), (3,5), (4,5), (5,5)]

heatMap :: Map.Map Location Heat
heatMap = undefined  

scoreBoard ::  GameState -> Float
scoreBoard gs@GameState{ getDisc, getBoard, getFrames } = undefined