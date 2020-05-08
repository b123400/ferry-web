module Scraping.NWFF.TimeStringSpec where

import Test.Hspec
import Scraping.NWFF.TimeString (parseTimeStr)

spec :: Spec
spec = do
    describe "Scraping.NWFF.TimeString.parseTimeStr" $ do

        it "parse time string 1" $ do
            ("", diffTime) <- parseTimeStr "04:15"
            diffTime `shouldBe` ((4 * 60 + 15) * 60)

        it "parse time string 2" $ do
            ("", diffTime) <- parseTimeStr "10:45"
            diffTime `shouldBe` ((10 * 60 + 45) * 60)

        it "parse time string with modifier 1" $ do
            ("#*", diffTime) <- parseTimeStr "#*19:40"
            diffTime `shouldBe` ((19 * 60 + 40) * 60)

        it "parse time string with modifier 2" $ do
            ("^*", diffTime) <- parseTimeStr "^*19:30"
            diffTime `shouldBe` ((19 * 60 + 30) * 60)
