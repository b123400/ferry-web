module Scraping.EmailDecode where

import Numeric (readHex)
import Data.Bits (xor)

-- https://usamaejaz.com/cloudflare-email-decoding/
decodeCloudFlareEmail :: String -> Either String String
decodeCloudFlareEmail str@(a:b:rest) =
    let base = readHex [a, b]
    in case base of
        ((base', _): _)-> loop base' rest
        _ -> Left $ "Cannot parse hex." <> str
    where
        loop base str@(a:b:rest) = do
            current <- case readHex [a, b] of
                ((str, _): _)-> Right $ toEnum $ xor str base
                _ -> Left $ "Cannot parse hex: " <> str
            r <- loop base rest
            pure (current : r)
        loop _ "" = Right []
        loop _ _ = Left $ "Invalid cf email: " <> str
