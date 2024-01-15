{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (zipWithM_)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Data.Version (Version (versionBranch))
import Utils (urlToFilename)
import DataModel (export)
import Fetch (fetch)
import Options.Applicative
import Paths_mealie_importer_haskell (version)
import Scrape (scrape)
import System.FilePath (takeDirectory)

data Command
    = ScrapeUrl String
    | UpdateTestdata
    | Version

parseArgs :: Parser Command
parseArgs =
    subparser
        ( command "scrape" (info scrapeUrl (progDesc ""))
            <> command "update-testdata" (info (pure UpdateTestdata) (progDesc ""))
        )
  where
    scrapeUrl :: Parser Command
    scrapeUrl =
        ScrapeUrl
            <$> argument
                str
                ( metavar "URL"
                    <> help "URL to scrape"
                )

run :: Command -> IO ()
run Version =
    putStrLn
        $ ("Version: " ++)
        $ intercalate "."
        $ map show
        $ versionBranch Paths_mealie_importer_haskell.version
run UpdateTestdata = do
    let configPath = "test/data/list.txt"
    urls <- lines <$> readFile configPath
    htmls <- mapM (fmap toString . fetch) urls
    let outputDir = takeDirectory configPath
    let outputFilenames = map (`urlToFilename` outputDir) urls
    zipWithM_ writeFile outputFilenames htmls
run (ScrapeUrl url') = do
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
