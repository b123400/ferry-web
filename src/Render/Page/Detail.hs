module Render.Page.Detail where

import Servant
import Lucid
import Control.Monad (forM_, when)
import Data.Aeson (ToJSON(..))
import Data.String (IsString(..))
import Data.Text (pack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import Render.Lang (Localised(..), Syllabus(..), translate, lShow)
import Render.Template.Wrapper (wrapper_)
import Render.Template.Timetable (timetable_)
import Timetable (Route(..), Island(..), dataSource, wikiLink)
import Timetable.Local (addDiff)
import Web.HttpApiData (toUrlPiece)

data Detail = Detail
    { now :: LocalTime
    , route :: Route LocalTime
    , date :: Day
    , count :: Int
    }

instance ToHtml (Localised Detail) where
    toHtmlRaw = toHtml
    toHtml (Localised l (Detail now route date count)) = wrapper_ l $ do
        a_ [class_ "back", href_ "/"] "↩︎"
        div_ [class_ "island --detail"] $ do
            h1_ (lShow l island)
            div_ [class_ "directions"] $ do
                forM_ timetables $ \t -> do
                    div_ [class_ "direction"] $ do
                        timetable_ l t
        div_ [class_ "sidebar"] $ do
            a_ [href_ $ "/" <> toUrlPiece island] (t Now)
            br_ []
            br_ []
            form_ [method_ "get"] $ do
                label_ [for_ "date"] (t SelectDate)
                br_ []
                input_ [type_ "date", name_ "date", value_ (toQueryParam date)]
                br_ []
                label_ [for_ "count"] (t ItemsPerPage)
                br_ []
                select_ [name_ "count"] $ do
                    option 10
                    option 20
                    option 50
                br_ []
                input_ [type_ "submit", value_ (t Submit)]
            br_ []
            br_ []
            a_ [href_ $ "/" <> toUrlPiece island <> "/raw"] (t RawTimetable)
            br_ []
            when (supportMetadata island) $ do
                a_ [href_ $ "/" <> toUrlPiece island <> "/metadata"] (t Metadata)
                br_ []
            a_ [href_ $ dataSource island, target_ "_blank", rel_ "noopener"] (t DataSource)
            br_ []
            a_ [href_ $ wikiLink island, target_ "_blank", rel_ "noopener"] (t Wiki)

        where withDiffs = addDiff now <$> route
              (Route island timetables) = withDiffs
              option c | c == count = option_ [value_ (pack $ show c), selected_ "true"] $ toHtml $ show c
                       | otherwise = option_ [value_ (pack $ show c)] $ toHtml $ show c
              t :: IsString s => Syllabus -> s
              t = translate l

instance ToJSON (Localised Detail) where
    toJSON (Localised l (Detail _ route _ _)) = toJSON route


supportMetadata :: Island -> Bool
supportMetadata CentralCheungChau = True
supportMetadata CentralMuiWo = True
supportMetadata CentralPengChau = True
supportMetadata CentralSokKwuWan = True
supportMetadata CentralYungShueWan = True
supportMetadata CentralMaWan = True
supportMetadata NorthPointHungHom = True
supportMetadata NorthPointKowloonCity = True
supportMetadata PengChauHeiLingChau = True
supportMetadata AberdeenSokKwuWan = True
supportMetadata MaWanTsuenWan = True
supportMetadata SaiWanHoKwunTong = True
supportMetadata SaiWanHoSamKaTsuen = True
supportMetadata _ = False