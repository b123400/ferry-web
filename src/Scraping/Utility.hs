module Scraping.Utility where

import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, following, node,
                        ($.//), ($//), (&|), ($/), (&/), (&//), (>=>))
import Data.Text (Text, pack, unpack)
import Scraping.EmailDecode (decodeCloudFlareEmail)
import Text.XML
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 (pack)

makeName :: String -> Text.XML.Name
makeName name = Text.XML.Name (T.pack name) Nothing Nothing

getTDs :: Cursor -> [Cursor]
getTDs tr = tr $/ (element "td")

hasTwoTd :: Cursor -> Bool
hasTwoTd cursor = length (getTDs cursor) == 2

flatContent :: Cursor -> Text
flatContent cursor =
    let cursorNode = node cursor
    in case cursorNode of
        (NodeContent cursorNode) -> T.concat $ content cursor
        {-
            Cloudflare's email protection is enabled and it messes with any element with the '@' character.
            e.g. <td>2.30 @</td> becomes <td>2.30 <a ... data-cfemail=...>email protection</a></td>
        -}
        (NodeElement (Element (Name "a" _ _) attrs _))
            | Just encodedEmail <- M.lookup "data-cfemail" attrs
            , Right parsed <- decodeCloudFlareEmail $ T.unpack encodedEmail
            -> T.pack parsed
        (NodeElement cursorNode) -> T.concat $ map flatContent $ child cursor
        _                        -> ""

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
