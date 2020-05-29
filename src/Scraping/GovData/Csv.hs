module Scraping.GovData.Csv where

import Control.Applicative ((<|>))
import Control.Newtype (Newtype, pack, unpack)
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import Data.Csv (decode, decodeByName, decodeWithP, defaultDecodeOptions, FromRecord(..), FromNamedRecord(..), FromField(..), HasHeader(..), Parser)
import Data.Maybe (catMaybes)
import Data.Set (Set, isSubsetOf, empty, insert)
import Data.Time.Clock (NominalDiffTime)
import GHC.Exts (toList)

import Timetable hiding (timetables)
import Scraping.GovData.TimeString (parseTimeStr)

parseCsv :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed, FromNamedRecord e)=> ByteString -> Either String [Timetable NominalDiffTime]
parseCsv bs = toTimetables @e @ed <$> (parseWithHeader @e @ed bs <|> parse @e @ed bs)

parseCsv' :: forall e ed di da t r i. (e ~ (Entry di da t r), NT di da t r ed, FromRecord i)=> (i -> Parser Bool) -> ByteString -> Either String [Timetable NominalDiffTime]
parseCsv' cond bs = toTimetables @e @ed <$> parse' @e @ed cond bs

data Entry di da t r = Entry
    { _direction :: di
    , _days :: da
    , _time :: t
    , _remark :: r
    }

type NTDirection di = Newtype di Direction
type NTDays da = Newtype da (Set Day)
type NTTime t = Newtype t NominalDiffTime
type NTRemark r = Newtype r ((Set Day, Set Modifier) -> (Set Day, Set Modifier))

type NT di da t r ed =
    ( NTDirection di
    , NTDays da
    , NTTime t
    , NTRemark r
    , EnumDays ed
    , FromRecord (Entry di da t r)
    )

class EnumDays a where
    enumerated :: [a]
    toDaySet :: a -> Set Day

emptyRemark :: (NTDays da)=> Entry di da t r -> (Set Day, Set Modifier)
emptyRemark (Entry _ d _ _) = (unpack d, empty)

addModifier :: Modifier -> (Set Day, Set Modifier) -> (Set Day, Set Modifier)
addModifier = second . insert

modifyDays :: (Set Day -> Set Day) -> (Set Day, Set Modifier) -> (Set Day, Set Modifier)
modifyDays = first

toTimetables :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed)=> [e] -> [Timetable NominalDiffTime]
toTimetables entries = do
    days <- enumerated
    direction <- [FromPrimary, ToPrimary]
    let ferries = toFerry @e @ed <$> findEntries direction days
    [Timetable ferries (toDaySet days) direction]

    where
        remarkedEntries = patchDaySetByRemark @e @ed <$> entries

        findEntries :: Direction -> ed -> [Entry di da t r]
        findEntries direction days = filter (match direction days) remarkedEntries

        match direction days (Entry direction' days' _ _) = direction == (unpack direction') && (matchDays days (unpack days'))

        matchDays :: ed -> Set Day -> Bool
        matchDays days days' = (toDaySet days) `isSubsetOf` days'


toFerry :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed)=> e -> Ferry NominalDiffTime
toFerry e@(Entry _ _ t r) =
    Ferry (unpack t) (snd $ (unpack r) $ emptyRemark e)

patchDaySetByRemark :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed)=> e -> Entry di da t r
patchDaySetByRemark e@(Entry direction' _ time' r) = Entry direction' (pack $ fst $ (unpack r) $ emptyRemark e) time' r

parseWithHeader :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed, FromNamedRecord e)=> ByteString -> Either String [Entry di da t r]
parseWithHeader bs = (toList . snd) <$> decodeByName bs

parse :: forall e ed di da t r. (e ~ (Entry di da t r), NT di da t r ed)=> ByteString -> Either String [e]
parse bs = toList <$> decode HasHeader bs

parse' :: forall e ed di da t r i. (e ~ (Entry di da t r), NT di da t r ed, FromRecord i)=> (i -> Parser Bool) -> ByteString -> Either String [e]
parse' cond bs = catMaybes <$> toList <$> decodeWithP decodeFn defaultDecodeOptions HasHeader bs
    where
        decodeFn record = do
            i <- parseRecord record
            isOk <- cond i
            if isOk
            then Just <$> parseRecord record
            else pure Nothing

tryFirstAsDirection :: forall (d :: *). (FromField d)=> [B.ByteString] -> Parser Bool
tryFirstAsDirection (bs: _) = (True <$ parseField @d bs) <|> (pure False)
tryFirstAsDirection _ = pure False
