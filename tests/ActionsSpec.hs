module ActionsSpec (spec) where

import Test.Hspec

import Actions
import Board
import Types

spec :: Spec
spec = do
    describe "flipCell" $ do
        it "returns (Just White) if (Just Black) is passed in." $
            flipCell (Just Black) `shouldBe` (Just White)
            
        it "returns (Just Black) if (Just Black) is passed in." $
            flipCell (Just White) `shouldBe` (Just Black)

        it "returns Nothing if Nothing is passed in." $
            flipCell Nothing `shouldBe` Nothing
        
    describe "flipDisc" $ do
        it "returns Black if White is passed in." $ 
            flipDisc Black `shouldBe` White

        it "returns White if Black is passed in." $
            flipDisc White `shouldBe` Black

    describe "isInside" $ do
        it "returns False if Location is not on the board." $
            isInside (9,0) `shouldBe` False

        it "returns True if Location is on the board." $
            isInside (3,5) `shouldBe` True

    describe "isEmptyCell" $ do 
        it "returns True if Nothing is passed in." $
            isEmptyCell Nothing `shouldBe` True

        it "returns False if (Just _) is passed in." $
            isEmptyCell (Just White)  `shouldBe` False
            
    describe "getMajorKeys" $ do
        it "returns [Location] if Location is passed in." $
            getMajorKeys (3,3) `shouldBe` [(0,0), (1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7)]

        it "returns [Location] if Location is passed in." $
            getMajorKeys (1,6) `shouldBe` [(0,5), (1,6), (2,7)]

        it "returns [Location] if Location is passed in." $
            getMajorKeys (7,0) `shouldBe` [(7,0)]
    
        it "returns [Location] if Location is passed in." $
            getMajorKeys (0,7) `shouldBe` [(0,7)]

    describe "flipCaptured" $ do
        it "returns [] if ([], []) is passed in." $ do
            flipCaptured ([],[]) `shouldBe` []

        it "returns [Cell] if ([], [Cell])" $ do
            flipCaptured ([], [Just Black, Just White]) `shouldBe` [Just Black, Just White]

    describe "getRowKeys" $ do
        it "returns [Location] if Location is passed in." $ do 
            getRowKeys (3,4) `shouldBe` [(0,4), (1,4), (2,4), (3,4), (4,4), (5,4), (6,4), (7,4)]

            getRowKeys (0,0) `shouldBe` [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]

    describe "makeMove" $ do 
        it "returns Board if Disc, Location, and Board is passed in." $ do
            makeMove Black (3,2) startingBoard `shouldBe` makeBoard [((3,2), Black), ((3,3), Black), ((4,3), Black), ((3,4), Black), ((4,4), White)] 

        it "returns Board if Disc, Location, and Board is passed in." $ do
            makeMove White (6,5) testBoard2 `shouldBe` makeBoard [((3,3), White), ((4,3), White), ((3,4), Black), ((4,4), Black), ((5,4), White), ((4,5), White), ((4,6), White), ((6,5), White)]