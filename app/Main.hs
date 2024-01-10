{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DataModel (export)
import Scrape (scrape)

main :: IO ()
main = do
    -- let url = "https://biancazapatka.com/de/veganes-naan-brot/"
    -- let url = "https://rainbowplantlife.com/pumpkin-salad/"
    let url = "https://rainbowplantlife.com/braised-tofu/#recipe"
    -- let url = "https://rainbowplantlife.com/baked-tofu/#recipe"
    -- let url = "https://www.bettybossi.ch/de/Rezept/ShowRezept/BB_APFI191014_0100A-60-de?title=Fregola-Sarda-Salat&list=c%3D%26f%3D-vegan"
    recipe <- scrape url
    case recipe of
        (Right r) -> putStrLn $ export r
        (Left err) -> putStrLn ("Error scraping '" ++ url ++ "': " ++ err)
