{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import API (About (production), getAbout, getUnits, getUnit, getFoods, Food (name))
import Control.Monad (zipWithM_)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate, sort)
import Data.Version (Version (versionBranch))
import DataModel (Recipe (recipeIngredient), export)
import Fetch (fetch)
import Options.Applicative
import Paths_mealie_importer_haskell (version)
import Scrape (scrape)
import System.FilePath (takeDirectory)
import Utils (urlToFilename)
import Data.List.Extra (trim)

data Command
    = ScrapeUrl !String
    | UpdateTestdata
    | Version
    | Test

parseArgs :: Parser Command
parseArgs =
    subparser
        ( command "scrape" (info scrapeUrl (progDesc "scrape a recipe from a website"))
            <> command "update-testdata" (info (pure UpdateTestdata) (progDesc "update the testdata list"))
            <> command "test" (info (pure Test) (progDesc "test command"))
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
    putStrLn $
        ("Version: " ++) $
            intercalate "." $
                map show $
                    versionBranch Paths_mealie_importer_haskell.version
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
run Test = do
    apiToken <- trim <$> readFile ".api-token"
    foods' <- getFoods apiToken
    putStrLn $ unlines $ sort $ map name foods'

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
