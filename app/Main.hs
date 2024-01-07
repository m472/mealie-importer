{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Fetch (fetch)

main :: IO ()
main = do
    body <- fetch "https://rainbowplantlife.com/pumpkin-salad/"
    L8.putStrLn body
