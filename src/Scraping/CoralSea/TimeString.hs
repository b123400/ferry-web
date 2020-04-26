module Scraping.CoralSea.TimeString where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Text.Parsec (ParsecT, Stream, char, digit, many1, parse, (<|>))
import Text.Parsec.Combinator (count)

parseTimeStr :: Monad m => Text -> m (NominalDiffTime, Bool)
parseTimeStr res =
    case parse timeStr "" res of
        Left error -> fail $ show error
        Right x -> pure x


timeStr :: Stream s m Char => ParsecT s u m (NominalDiffTime, Bool)
timeStr = (,) <$> time <*> modifier

modifier :: Stream s m Char => ParsecT s u m Bool
modifier = (char '^') $> True <|> pure False

time :: Stream s m Char => ParsecT s u m NominalDiffTime
time = do
    hour <- read <$> many1 digit
    char ':'
    minute <- read <$> many1 digit
    pure $ fromInteger $ ((hour * 60) + minute) * 60
