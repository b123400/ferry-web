module Render.Page.Metadata where

import Servant
import Lucid
import Control.Monad (forM_, when)
import Data.Aeson (ToJSON(..))
import Data.List (any)
import Data.Maybe (Maybe(..))
import Data.Time.Clock (NominalDiffTime)
import Data.Set (toList)
import Render.Lang as L (Localised(..), translate, lShow)
import qualified Render.Lang as L
import Render.DaysName (showDays)
import Render.Template.Wrapper (wrapper_)
import Render.Template.RawTimetable (rawTimetable_)
import Timetable (Route(..), Timetable(..), Day(..), Modifier(..), Island, secondaryName)
import qualified Timetable.Metadata as Model

data Metadata = Metadata Island Model.Metadata

instance ToHtml (Localised Metadata) where
    toHtmlRaw = toHtml
    toHtml (Localised l (Metadata island (Model.Metadata fares durations))) = wrapper_ l $ do
        a_ [class_ "back", href_ ("/" <> toUrlPiece island)] "↩"
        div_ [class_ "metadata --raw"] $ do
            h1_ (lShow l island)
            div_ [class_ "fares"] $ do
                table_ $ do
                    tr_ $ do
                        th_ $ translate l L.Passenger
                        th_ $ translate l L.Fare
                        th_ $ translate l L.Type
                        th_ $ translate l L.Extras
                    forM_ fares $ \fare->
                        tr_ $ do
                            td_ (toHtml $ Model.passenger fare)
                            td_ (toHtml $ "$" <> (show $ Model.fare fare))
                            td_ $ showFareType (Model.type_ fare)
                            td_ $
                                forM_ (toList $ Model.modifiers fare) $ \modifier->
                                    span_ [class_ "metadata__modifier"] (toHtml $ showModifier l island modifier)
            hr_ []
            div_ [class_ "durations"] $ do
                table_ $ do
                    tr_ $ do
                        th_ $ translate l L.Type
                        th_ "Duration"
                    forM_ durations $ \duration->
                        tr_ $ do
                            td_ (toHtml $ showDurationType $ Model.ferryType duration)
                            td_ (toHtml $ showDuration $ Model.duration duration)
        where
            isAnyNotSlow = any (/= SlowFerry) (Model.ferryTypeToTimetableModifier <$> Model.type_ <$> fares)
            showFareType type_ = do
                when isAnyNotSlow $ span_ [class_ ("metadata__fare-type " <> classForFareType type_)] $ toHtml $ translateFareType l type_
                forM_ (extraTextForFareType l type_) $ \t-> span_ [class_ "metadata__fare-type"] (toHtml t)
            classForFareType t = case (Model.ferryTypeToTimetableModifier t) of
                FastFerry -> "--fast-ferry"
                SlowFerry -> "--slow-ferry"
            showDurationType (Just t) = translateFerryType l t
            showDurationType _ = "-"
            showDuration :: NominalDiffTime -> String
            showDuration t = (show $ round (t / 60)) <> (translate l L.Minutes)

showModifier :: L.Lang -> Island -> Model.Modifier -> String
showModifier L.En island Model.FromSecondaryOnly = "From " <> (secondaryName L.En island) <> " only"
showModifier L.En island Model.RegisteredUser = "For Registered Users only"
showModifier L.Hk island Model.FromSecondaryOnly = "只限由" <> (secondaryName L.Hk island) <> "出發"
showModifier L.Hk island Model.RegisteredUser = "只限已登記用戶"

translateFareType :: L.Lang -> Model.FareType -> String
translateFareType l = translateFerryType l . Model.ferryTypeToTimetableModifier

translateFerryType :: L.Lang -> Modifier -> String
translateFerryType l m = case m of
    FastFerry -> L.translate l L.FastFerry
    SlowFerry -> L.translate l L.SlowFerry

extraTextForFareType :: L.Lang -> Model.FareType -> Maybe String
extraTextForFareType l Model.SlowFerryDeluxeClass = Just $ L.translate l L.DeluxeClass
extraTextForFareType _ _ = Nothing

instance ToJSON (Localised Metadata) where
    toJSON (Localised _ (Metadata _ m)) = toJSON m
