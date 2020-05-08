module Scraping.Gov.TimeStringSpec where

import Test.Hspec
import Scraping.Gov.TimeString (parseTimeStr)

spec :: Spec
spec = do
    describe "Scraping.Gov.TimeString.parseTimeStr" $ do

        it "parse time string 1" $ do
            (diffTime, "") <- parseTimeStr "10.00 a.m."
            diffTime `shouldBe` ((10 * 60 + 0) * 60)

        it "parse time string 2" $ do
            (diffTime, "") <- parseTimeStr "7.45 am"
            diffTime `shouldBe` ((7 * 60 + 45) * 60)

        it "parse time string 3" $ do
            (diffTime, "") <- parseTimeStr "1.45 pm"
            diffTime `shouldBe` ((13 * 60 + 45) * 60)

        it "parse time string 4" $ do
            (diffTime, "") <- parseTimeStr "12.00 noon"
            diffTime `shouldBe` ((12 * 60 + 0) * 60)

        it "parse time string 5" $ do
            (diffTime, "") <- parseTimeStr "12:00 noon"
            diffTime `shouldBe` ((12 * 60 + 0) * 60)

        it "parse time string 6" $ do
            (diffTime, "") <- parseTimeStr "12.00 a.m."
            diffTime `shouldBe` 0

        it "parse time string 7" $ do
            (diffTime, "") <- parseTimeStr "12.00 p.m."
            diffTime `shouldBe` ((12 * 60 + 0) * 60)

        it "parse time string 8" $ do
            (diffTime, "") <- parseTimeStr "8.45p.m."
            diffTime `shouldBe` ((20 * 60 + 45) * 60)

        it "parse time string with modifier 1" $ do
            (diffTime, "*@") <- parseTimeStr "7.30 p.m.*@"
            diffTime `shouldBe` ((19 * 60 + 30) * 60)

        it "parse time string with modifier 2" $ do
            (diffTime, "*") <- parseTimeStr "12.00 noon*"
            diffTime `shouldBe` ((12 * 60 + 0) * 60)

        it "parse time string with modifier 3" $ do
            (diffTime, "#") <- parseTimeStr "12.00 noon #"
            diffTime `shouldBe` ((12 * 60 + 0) * 60)
