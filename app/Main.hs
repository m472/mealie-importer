{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DataModel (export)
import Options.Applicative
import Scrape (scrape)

data CliOptions = CliOptions
    { url :: String
    , verbose :: Bool
    , version :: Bool
    }

parseArgs :: Parser CliOptions
parseArgs =
    CliOptions
        <$> argument
            str
            ( metavar "URL"
                <> help "URL to scrape"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "additional output"
            )
        <*> switch
            ( long "version"
                <> help "print version number and exit"
            )

run :: CliOptions -> IO ()
run (CliOptions{url = _, verbose = _, version = True}) = putStrLn ""
run CliOptions{url = url', verbose = _} = do
    recipe <- scrape url'
    case recipe of
        (Right r) -> putStrLn $ export r
        (Left err) -> putStrLn ("Error scraping '" ++ url' ++ "': " ++ err)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
        info
            (parseArgs <**> helper)
            ( fullDesc
                <> progDesc "Recipe importer for mealie"
                <> header "mealie-importer"
            )
