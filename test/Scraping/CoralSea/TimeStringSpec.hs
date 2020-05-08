module Scraping.CoralSea.TimeStringSpec where

import Test.Hspec
import Scraping.CoralSea.TimeString (parseTimeStr)

spec :: Spec
spec = do
    describe "Scraping.CoralSea.TimeString.parseTimeStr" $ do

        it "parse time string 1" $ do
            (diffTime, False) <- parseTimeStr "21:18"
            diffTime `shouldBe` ((21 * 60 + 18) * 60)

        it "parse time string 2" $ do
            (diffTime, False) <- parseTimeStr "7:33"
            diffTime `shouldBe` ((7 * 60 + 33) * 60)

        it "parse time string with modifier" $ do
            (diffTime, True) <- parseTimeStr "9:03^"
            diffTime `shouldBe` ((9 * 60 + 3) * 60)
