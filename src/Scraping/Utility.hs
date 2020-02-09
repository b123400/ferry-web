module Scraping.Utility where

import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, following, node,
                        ($.//), ($//), (&|), ($/), (&/), (&//), (>=>))
import Data.Text (Text, pack, unpack)
import Text.XML
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 (pack)
import Control.Arrow

makeName :: String -> Text.XML.Name
makeName name = Text.XML.Name (T.pack name) Nothing Nothing

makeElement name = element $ makeName name

getTDs :: Cursor -> [Cursor]
getTDs tr = tr $/ (makeElement "td")

hasTwoTd :: Cursor -> Bool
hasTwoTd cursor = length (getTDs cursor) == 2

flatContent :: Cursor -> Text
flatContent cursor =
    let cursorNode = node cursor
    in case cursorNode of
        (NodeContent cursorNode) -> T.concat $ content cursor
        {-
            Ususally the table looks like this:
            <td>11.30</td>
            <td>12.30</td>
            But there is a fucking line looks like this:
            <td>11.30</td>
            <td><p>12.30</p></td>
            So we have to handle that case
        -}
        (NodeElement cursorNode) -> T.concat $ map flatContent $ child cursor
        _                        -> error "not supported element type"

nthMatch :: Int -> (Node -> Bool) -> [Cursor] -> Cursor
nthMatch nth _ [] = error ("not found in empty list")
nthMatch nth matcher (x:xs) 
    | matches && (nth == 1) = x
    | matches && (nth >  1) = nthMatch (nth - 1) matcher xs
    | otherwise             = nthMatch nth matcher xs
    where matches = matcher $ node x

matchName :: String -> (Node -> Bool)
matchName name (NodeElement node) = (elementName node) == (makeName name)
matchName _ _ = False

notEmpty :: Text -> Bool
notEmpty text = text /= (pack "\160") && text /= (pack "\r\n")

pickOdd :: [a] -> [a]
pickOdd [] = []
pickOdd (x:xs) = [x] ++ pickEven xs

pickEven :: [a] -> [a]
pickEven [] = []
pickEven (x:xs) = pickOdd xs

{-
Fix this the &nbsp; shit
-}
cleanHTMLEntity :: String -> String
cleanHTMLEntity = map (\c-> if c==' 'then ' ' else c)