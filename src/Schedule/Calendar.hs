{-# LANGUAGE TemplateHaskell #-}
module Schedule.Calendar (HolidayCalendar, isHoliday, parseICal) where

import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time.Calendar (Day, fromGregorian)
import Text.Parsec
import Text.Parsec.Combinator


type HolidayCalendar = [Holiday]

data EventEntry = Date Integer Int Int | Summary String | Other deriving (Show)
data Holiday = Holiday
    { day :: Day
    , name :: String
    } deriving (Show)

$(deriveJSON defaultOptions ''EventEntry)
$(deriveJSON defaultOptions ''Holiday)

isHoliday :: HolidayCalendar -> Day -> Bool
isHoliday holidays day = any (\(Holiday d _)-> d == day) holidays

parseICal :: (Monad m, MonadFail m) => ByteString -> m HolidayCalendar
parseICal res =
    case parse ical "" res of
        Left error -> fail $ show error
        Right x -> pure x

-- An incompleted parser for the iCal format.
-- The one published by hkgov is invalid and cannot be parsed by a proper parser.
-- Fuck hk gov.
ical :: Stream s m Char => ParsecT s u m [Holiday]
ical = do
    lineVal "BEGIN" "VCALENDAR"
    _ <- prelude
    events <- many event
    lineVal "END" "VCALENDAR"
    pure events

prelude :: Stream s m Char => Stream s m Char => ParsecT s u m [String]
prelude = manyTill p (try $ lookAhead beginEvent)
    where p = (try $ line "PRODID")
              <|> (try $ line "VERSION")
              <|> (try $ line "CALSCALE")
              <|> (try $ line "X-WR-TIMEZONE")
              <|> (try $ line "X-WR-CALNAME")
              <|> (try $ line "X-WR-CALDESC")

beginEvent :: Stream s m Char => ParsecT s u m ()
beginEvent = lineVal "BEGIN" "VEVENT"

endEvent :: Stream s m Char => ParsecT s u m ()
endEvent = lineVal "END" "VEVENT"

event :: Stream s m Char => ParsecT s u m Holiday
event = do
    beginEvent
    bodies <- many body
    endEvent

    let h = Holiday <$> (listToMaybe $ mapMaybe getDate bodies)
                    <*> (listToMaybe $ mapMaybe getSummary bodies)
    case h of
        Nothing -> fail "Cannot parse event"
        Just h -> pure h

    where body = (try $ dateLine)
             <|> Other   <$  (try $ line "DTEND;VALUE=DATE")
             <|> Other   <$  (try $ line "TRANSP")
             <|> Other   <$  (try $ line "UID")
             <|> Summary <$> (try $ line "SUMMARY")

          getDate (Date y m d) = Just $ fromGregorian y m d
          getDate _ = Nothing
          getSummary (Summary str) = Just str
          getSummary _ = Nothing

          dateLine = do
              string "DTSTART;VALUE=DATE:"
              yearStr <- count 4 digit
              monthStr <- count 2 digit
              dayStr <- count 2 digit
              endOfLine
              pure $ Date (read yearStr) (read monthStr) (read dayStr)

lineVal :: Stream s m Char => String -> String -> ParsecT s u m ()
lineVal prefix val = do
    string prefix
    char ':'
    string val
    (endOfLine <|> ('.' <$ eof))
    pure ()

line :: Stream s m Char => String -> ParsecT s u m String
line prefix = do
    string prefix
    char ':'
    manyTill anyChar (endOfLine <|> ('.' <$ eof))
