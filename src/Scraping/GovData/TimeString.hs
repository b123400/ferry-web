module Scraping.GovData.TimeString where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Text.Parsec (ParsecT, Stream, char, digit, many, many1, optional, oneOf, parse, spaces, string, try, (<|>))
import Text.Parsec.Combinator (count)


parseTimeStr :: (Monad m, MonadFail m) => Text -> m NominalDiffTime
parseTimeStr res =
    case parse time "" res of
        Left error -> fail $ show error
        Right x -> pure x

data Period = AM | PM | Noon

time :: Stream s m Char => ParsecT s u m NominalDiffTime
time = do
    hour <- read <$> many1 digit
    char ':' <|> char '.'
    minute <- read <$> many1 digit
    spaces
    ampm <- period
    let extraHour = case ampm of
                        AM -> 0
                        PM -> 12
                        Noon -> 12 -- 12 noon -> (12 mod 12) noon -> 0 noon -> 12
    pure $ fromInteger $ ((extraHour + (hour `mod` 12)) * 60 + minute) * 60

period :: Stream s m Char => ParsecT s u m Period
period = try ampm <|> try noon
    where
        noon = string "noon" $> Noon
        ampm = do
            c <- char 'a' <|> char 'p'
            optional $ char '.'
            char 'm'
            optional $ char '.'
            pure $ case c of
                'a' -> AM
                'p' -> PM
