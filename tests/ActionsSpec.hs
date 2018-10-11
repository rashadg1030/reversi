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

