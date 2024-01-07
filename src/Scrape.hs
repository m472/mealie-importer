module Scrape (
    scrape,
    Receipe,
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Html.Scalpel

newtype Receipe = Receipe {title :: String}

scrape :: L8.ByteString -> Receipe
scrape htmlString = Receipe{title = "Test"}
