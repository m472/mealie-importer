{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DataModel (export)
import Scrape (scrape)

main :: IO ()
main = do
    -- recipe <- scrape "https://rainbowplantlife.com/pumpkin-salad/"
    recipe <- scrape "https://rainbowplantlife.com/braised-tofu/#recipe"
    -- recipe <- scrape "https://www.bettybossi.ch/de/Rezept/ShowRezept/BB_APFI191014_0100A-60-de?title=Fregola-Sarda-Salat&list=c%3D%26f%3D-vegan"
    case recipe of
        (Just r) -> putStrLn $ export r
        Nothing -> putStrLn "Could not scrape"
