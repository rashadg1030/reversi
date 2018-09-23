module Test where

import Main

main :: IO ()
main = undefined

testChangeCellRow = (changeCellRow Black 3 [Empty, Empty, Empty, White, Black, Empty, Empty, Empty]) == [Empty, Empty, Empty, Black, Black, Empty, Empty, Empty]

testDisplayBoard = displayBoard startingBoard

testChangeCell =  displayBoard $ changeCell White (5,6) startingBoard

testIsSameCellMap = (isSameCellMap White [Black, White, White, Empty, Empty, Black, White]) == [False, True, True, False, False, False, True]

testShaveRow = (shaveRow 1 [White, Empty, White, White, Black, White, White, White]) == [White,White,Black,White,White,White]

testIsOppositeCell = ([(isOppositeCell Black White), (isOppositeCell Black Black), (isOppositeCell White Empty), (isOppositeCell Empty Empty)]) == [True, False, False, False]

{--
divideRow Tests: inputs are shaved rows
    divideRow Black [White, White, Black, White, White, White]
    divideRow Black [Empty, White, Black, White, White, White]
    divideRow Black [Black, Empty, Black, White, White, White]
    divideRow White [Black, Black, Black, Black, White, Black]
    divideRow White [Black, Black, Black, Black, Black, White]
    divideRow White [Black, Black, Black, Black, Black, Black]
--}