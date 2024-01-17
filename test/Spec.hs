import Control.Monad (zipWithM_)
import Scrape (parseRecipe)
import System.FilePath (takeDirectory)
import Text.HTML.Scalpel (URL)
import Utils (urlToFilename)

main :: IO ()
main = do
    let testListPath = "test/data/list.txt"
    urls <- lines <$> readFile testListPath
    fileContents <- mapM (readFile . (`urlToFilename` takeDirectory testListPath)) urls
    zipWithM_ test urls fileContents
  where
    test :: URL -> String -> IO ()
    test url html = case parseRecipe url html of
        (Left _) -> error "this is an error"
        (Right _) -> putStrLn ("Success: " ++ url)
