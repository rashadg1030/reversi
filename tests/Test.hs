module Test where

import Main

main :: IO ()
main = undefined

testChangeCellRow = (changeCellRow Black 3 [Empty, Empty, Empty, White, Black, Empty, Empty, Empty]) == [Empty, Empty, Empty, Black, Black, Empty, Empty, Empty]

testDisplayBoard = displayBoard startingBoard

testChangeCell =  displayBoard $ changeCell White (5,6) startingBoard

testIsSameCellMap = (isSameCellMap White [Black, White, White, Empty, Empty, Black, White]) == [False, True, True, False, False, False, True]

--testShaveRow = (shaveRow 1 [White, Empty, White, White, Black, White, White, White]) == [White,White,Black,White,White,White]

testIsOppositeCell = ([(isOppositeCell Black White), (isOppositeCell Black Black), (isOppositeCell White Empty), (isOppositeCell Empty Empty)]) == [True, False, False, False]

-- let stepOne = shaveRow 2 [Empty, Empty, Empty, White, Black, Empty, Empty, Empty]
-- let left = reverse $ first stepOne
-- let right = snd stepOne
-- leftDivided = ([],[Empty,Empty])
-- rightDivided = ([White],[Black,Empty,Empty,Empty])
-- let newLeft = reverse $ flipCaptured leftDivided
-- let newRight = flipCaptured rightDivided
-- let canPlay = (checkRowPair leftDivided) || (checkRowPair rightDivided)

-- Passes:
-- playHorizontal Black 4 [White, White, White, White, Empty, White, Empty, Black]
-- playHorizontal Black 5 [Empty, White, White, Black, White, Empty, White, Black]
-- playHorizontal Black 4 [Black, White, White, White, Empty, White, Empty, Black]
-- playHorizontal Black 4 [Empty, White, White, White, Empty, White, White, Black]

-- Doesn't pass:
-- playHorizontal White 6 [Black, White, White, White, Black, Black, Empty, Black]



--

{--
divideRow Tests: inputs are shaved rows
divideRow Black [White, White, Black, White, White, White]
divideRow Black [Empty, White, Black, White, White, White]
divideRow Black [Black, Empty, Black, White, White, White]
divideRow White [Black, Black, Black, Black, White, Black]
divideRow White [Black, Black, Black, Black, Black, White]
divideRow White [Black, Black, Black, Black, Black, Black]
--}

{--
checkRowPair Test: inputs are a pair of rows from divideRow

Should be False:
checkRowPair $ divideRow Black [Black, Empty, Black, White, White, White]
checkRowPair $ divideRow White [White, Empty, White, White, White]
checkRowPair $ divideRow Black [Black, Empty]
checkRowPair $ divideRow White [Black, Black, Black, Black, Black, Black]
checkRowPair $ divideRow White [Black, Empty, Black, Black, Black, Black]
checkRowPair $ divideRow White [Empty, Empty, Black, Black, Black, Black]

Should be True:
checkRowPair $ divideRow White [Black, Black, Black, Black, Black, White]
checkRowPair $ divideRow Black [White, White, Black, White, White, White]
checkRowPair $ divideRow Black [White, Black, Black, White, White]
--}

-- playXY White (3,5) startingBoard
-- playXY Black (2,3) (playXY White (3,5) startingBoard)