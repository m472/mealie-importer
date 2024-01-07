module Scrape (
    Scrape.scrape,
) where

import Data.ByteString.Lazy.UTF8 (toString)
import Data.List.Extra (trim)
import DataModel (Recipe (..), RecipeIngredient (..))
import Debug.Trace (traceShowId)
import Fetch (fetch, getDomain)
import Text.HTML.Scalpel
import Text.Read (readMaybe)
import Text.StringLike (StringLike)

addOrgUrl :: URL -> Recipe -> Recipe
addOrgUrl url recipe = recipe{orgURL = Just url}

scrape :: URL -> IO (Maybe Recipe)
scrape url = do
    let scrapeMethod = case getDomain url of
            (Just "rainbowplantlife.com") -> scrapeWPRM
            -- (Just "bettybossi.ch") -> scrapeBettyBossi
            _ -> const Nothing
    htmlSource <- Fetch.fetch url
    return $ addOrgUrl url <$> scrapeMethod (toString htmlSource)

extractTimeWPRM :: (StringLike str, Monad m) => String -> ScraperT str m str
extractTimeWPRM time =
    text
        $ TagString "div"
        @: [hasClass ("wprm-recipe-" ++ time ++ "-time-container")]
        // TagString "span"
        @: [hasClass ("wprm-recipe-" ++ time ++ "_time")]

readQuantity :: String -> Maybe Float
readQuantity "⅛" = Just 0.125
readQuantity "¼" = Just 0.25
readQuantity "½" = Just 0.5
readQuantity "¾" = Just 0.75
readQuantity s = readMaybe s

scrapeWPRM :: String -> Maybe Recipe
scrapeWPRM htmlString = scrapeStringLike htmlString recipe
  where
    recipe :: Scraper String Recipe
    recipe = do
        recipeTitle <- text $ TagString "h2" @: [hasClass "wprm-recipe-name"]
        prepTime' <- extractTimeWPRM "prep"
        totalTime' <- extractTimeWPRM "total"
        cookTime' <- extractTimeWPRM "cook"
        description' <- text $ TagString "div" @: [hasClass "wprm-recipe-summary"]
        ingredients' <- ingredients
        return
            $ Recipe
                { name = recipeTitle
                , orgURL = Nothing
                , prepTime = prepTime'
                , totalTime = totalTime'
                , cookTime = cookTime'
                , description = description'
                , recipeIngredient = concat ingredients'
                }

    ingredients :: Scraper String [[RecipeIngredient]]
    ingredients = chroots (TagString "div" @: [hasClass "wprm-recipe-ingredient-group"]) ingredientGroup

    ingredientGroup :: Scraper String [RecipeIngredient]
    ingredientGroup = do
        groupName <- text $ TagString "h4" @: [hasClass "wprm-recipe-ingredient-group-name"]
        chroots (TagString "li" @: [hasClass "wprm-recipe-ingredient"]) (ingredient groupName)

    ingredient :: String -> Scraper String RecipeIngredient
    ingredient groupName = do
        amount <- text $ TagString "span" @: [hasClass "wprm-recipe-ingredient-amount"]
        unit' <- text $ TagString "span" @: [hasClass "wprm-recipe-ingredient-unit"]
        food' <- text $ TagString "span" @: [hasClass "wprm-recipe-ingredient-name"]
        note' <- text $ TagString "span" @: [hasClass "wprm-recipe-ingredient-notes"]
        origText' <- text $ TagString "li" @: [hasClass "wprm-recipe-ingredient"]
        pos <- position

        let cleanOrigText = unwords . filter (/= "▢") . map trim . words
        let title' = case pos of
                        0 -> Just groupName
                        _ -> Nothing

        return
            $ case readQuantity amount of
                Nothing ->
                    RecipeIngredient
                        { quantity = Nothing
                        , unit = Nothing
                        , food = amount ++ " " ++ unit' ++ " " ++ food'
                        , note = note'
                        , originalText = cleanOrigText origText'
                        , title = title'
                        }
                (Just parsedAmount) ->
                    RecipeIngredient
                        { quantity = Just parsedAmount
                        , unit = Just unit'
                        , food = food'
                        , note = note'
                        , originalText = cleanOrigText origText'
                        , title = title'
                        }

{-
scrapeBettyBossi :: String -> Maybe Recipe
scrapeBettyBossi htmlString = scrapeStringLike htmlString recipe
  where
    recipe :: Scraper String Recipe
    recipe = do
        recipeTitle <- text $ TagString "h1" @: [hasClass "title"]
        return $ Recipe{name = recipeTitle, orgURL = Nothing}
        -}
