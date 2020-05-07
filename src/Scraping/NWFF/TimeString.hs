module Scraping.NWFF.TimeString where

import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Text.Parsec (ParsecT, Stream, char, digit, many, parse, (<|>))
import Text.Parsec.Combinator (count)

parseTimeStr :: (Monad m, MonadFail m) => Text -> m (String, NominalDiffTime)
parseTimeStr res =
    case parse timeStr "" res of
        Left error -> fail $ show error
        Right x -> pure x


timeStr :: Stream s m Char => ParsecT s u m (String, NominalDiffTime)
timeStr = (,) <$> many modifier <*> time

modifier :: Stream s m Char => ParsecT s u m Char
modifier = char '#'
       <|> char '*'

time :: Stream s m Char => ParsecT s u m NominalDiffTime
time = do
    hour <- read <$> count 2 digit
    char ':'
    minute <- read <$> count 2 digit
    pure $ fromInteger $ ((hour * 60) + minute) * 60
