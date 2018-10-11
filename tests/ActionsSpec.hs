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
        it "returns False if the location is not on the board." $
            isInside (9,0) `shouldBe` False

        it "returns True if the location is on the board." $
            isInside (3,5) `shouldBe` True

    describe "isEmptyCell" $ do 
        it "returns True if Nothing is passed in." $
            isEmptyCell Nothing `shouldBe` True

        it "returns False if (Just _) is passed in." $
            isEmptyCell (Just White)  `shouldBe` False
            
            
    


    
