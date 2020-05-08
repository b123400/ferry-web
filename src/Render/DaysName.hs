module Render.DaysName where

import Data.Time.Calendar (DayOfWeek(..))
import Data.List (last, intercalate)
import Data.Set (Set, toAscList)
import Timetable (Day(..))
import Render.Lang (Lang(..), lShow)

-- Turn days into groups
-- e.g. [Mon, Tue, Wed, Sun] -> [Right (Mon, Wed), Left Sun]
-- So we can render "Mon to Wed, and Sun"
nameGroups :: Set Day -> [Either Day (Day, Day)]
nameGroups = fmap toEither . ascListToGroups . toAscList
    where
        ascListToGroups :: [Day] -> [[Day]]
        ascListToGroups [] = []
        ascListToGroups (onlyDay:[]) = [[onlyDay]]
        ascListToGroups (curr:rest) =
            case ascListToGroups rest of
                [] -> [[curr]]
                (firstGroup@(first: _):remainder)
                    | first /= Holiday && succ curr == first -> (curr : firstGroup) : remainder
                    | otherwise                              -> [curr] : firstGroup : remainder
                ([]: remainder) -> [curr] : remainder

        toEither :: [Day] -> Either Day (Day, Day)
        toEither (only: []) = Left only
        toEither (first: rest) = Right (first, last rest)

showDays :: Lang -> Set Day -> String
showDays lang = tGroups lang . nameGroups
    where
        tPeriod En (d1, d2) = (lShow En d1) <> " to " <> (lShow En d2)
        tPeriod Hk (d1, d2) = (lShow Hk d1) <> " 至 " <> (lShow Hk d2)

        tGroup l (Left d) = lShow l d
        tGroup l (Right dd) = tPeriod l dd

        tGroups l [] = ""
        tGroups l (only:[]) = tGroup l only
        tGroups En groups = let texts = tGroup En <$> groups
                            in (intercalate ", " $ init texts) <> " and " <> last texts
        tGroups Hk groups = let texts = tGroup Hk <$> groups
                            in (intercalate "，" $ init texts) <> " 及 " <> last texts
