{-# LANGUAGE OverloadedStrings #-}

module Scrape (
    Scrape.scrape,
    parseRecipe,
) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List.Extra (trim)
import DataModel (
    Recipe (..),
    RecipeIngredient (..),
    RecipeInstruction (..),
 )
import Debug.Trace (trace, traceShowId)
import Fetch (fetch, getDomain)
import Text.HTML.Scalpel
import Text.Read (readMaybe)

type Error = String
type ScraperWithError a = ScraperT String (Writer [Error]) a

addOrgUrl :: URL -> Recipe -> Recipe
addOrgUrl url recipe = recipe{orgURL = Just url}

parseRecipe :: URL -> String -> Either Error Recipe
parseRecipe url =
    case getDomain url of
        (Just "rainbowplantlife.com") -> scrapeWPRM
        (Just "biancazapatka.com") -> scrapeWPRM
        -- (Just "bettybossi.ch") -> scrapeBettyBossi
        _ -> const (Left "homepage not (yet) supported")

scrape :: URL -> IO (Either Error Recipe)
scrape url = do
    htmlSource <- Fetch.fetch url
    return $ addOrgUrl url <$> parseRecipe url (toString htmlSource)

extractTimeWPRM :: String -> ScraperWithError String
extractTimeWPRM time =
    text
        ( TagString "div"
            @: [hasClass ("wprm-recipe-" ++ time ++ "-time-container")]
            // TagString "span"
            @: [hasClass ("wprm-recipe-" ++ time ++ "_time")]
        )
        <|> pure ""

readQuantity :: String -> Maybe Float
readQuantity "⅛" = Just 0.125
readQuantity "¼" = Just 0.25
readQuantity "½" = Just 0.5
readQuantity "¾" = Just 0.75
readQuantity s = readMaybe s

logError :: String -> ScraperWithError a
logError message = do
    currentHtml <- html anySelector
    tell [message ++ " " ++ currentHtml]
    empty

scrapeStringOrError :: String -> ScraperWithError a -> (Maybe a, [Error])
scrapeStringOrError html' scraper = runWriter $ scrapeStringLikeT html' scraper

processResult :: (Maybe a, [Error]) -> Either Error a
processResult result = case result of
    (Just value, []) -> Right value
    (Just value, errors) -> trace ("Scraping produced result but errors were not empty: " ++ show errors) (Right value)
    (Nothing, errors) -> Left $ unlines errors

scrapeWPRM :: String -> Either Error Recipe
scrapeWPRM htmlString = processResult $ scrapeStringOrError htmlString recipe
  where
    recipe :: ScraperWithError Recipe
    recipe = do
        recipeTitle <- text ("h2" @: [hasClass "wprm-recipe-name"]) <|> logError "could not parse title"
        prepTime' <- extractTimeWPRM "prep"
        totalTime' <- extractTimeWPRM "total"
        cookTime' <- extractTimeWPRM "cook"
        description' <- text $ "div" @: [hasClass "wprm-recipe-summary"]
        ingredients' <- ingredients
        instructions' <- instructions
        return
            $ Recipe
                { name = recipeTitle
                , orgURL = Nothing
                , prepTime = prepTime'
                , totalTime = totalTime'
                , cookTime = cookTime'
                , description = description'
                , recipeIngredient = concat ingredients'
                , recipeInstruction = concat instructions'
                }

    ingredients :: ScraperWithError [[RecipeIngredient]]
    ingredients = chroots ("div" @: [hasClass "wprm-recipe-ingredient-group"]) ingredientGroup

    ingredientGroup :: ScraperWithError [RecipeIngredient]
    ingredientGroup = do
        groupName <- text ("h4" @: [hasClass "wprm-recipe-ingredient-group-name"]) <|> pure ""
        chroots ("li" @: [hasClass "wprm-recipe-ingredient"]) (ingredient groupName)

    ingredient :: String -> ScraperWithError RecipeIngredient
    ingredient groupName = do
        amount <- text $ "span" @: [hasClass "wprm-recipe-ingredient-amount"]
        unit' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-unit"]
        food' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-name"]
        note' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-notes"]
        origText' <- text $ "li" @: [hasClass "wprm-recipe-ingredient"]
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

    instructions :: ScraperWithError [[RecipeInstruction]]
    instructions = chroots ("div" @: [hasClass "wprm-recipe-instruction-group"]) instructionGroup

    instructionGroup :: ScraperWithError [RecipeInstruction]
    instructionGroup = do
        groupName <- text ("h4" @: [hasClass "wprm-recipe-instruction-group-name"]) <|> pure ""
        chroots ("li" @: [hasClass "wprm-recipe-instruction"]) (instruction groupName)

    instruction :: String -> ScraperWithError RecipeInstruction
    instruction groupName = do
        text' <- text $ "div" @: [hasClass "wprm-recipe-instruction-text"]
        pos <- position
        return
            $ RecipeInstruction
                { instructionText = text'
                , instructionTitle = if pos == 0 then groupName else ""
                }

{-
scrapeBettyBossi :: String -> Either Error Recipe
scrapeBettyBossi htmlString = processResult $ scrapeStringOrError htmlString recipe
  where
    recipe :: ScraperWithError Recipe
    recipe = do
        recipeTitle <- text $ "h1" @: [hasClass "title"]
        prepTime' <- text $ "div" @: map hasClass ["icon-duration", "bb-tag", "bb-icon-tag"]
        description' <- text $ "p" @: [hasClass "description"]
        ingredients' <- ingredients
        instructions' <- instructions
        return
            $ Recipe
                { name = recipeTitle
                , orgURL = Nothing
                , prepTime = prepTime'
                , totalTime = ""
                , cookTime = ""
                , description = description'
                , recipeIngredient = ingredients'
                , recipeInstruction = instructions'
                }

    ingredients :: ScraperWithError [RecipeIngredient]
    ingredients = chroots ("section" @: [hasClass "bb-recipe-ingredients"]) ingredient

    ingredient :: ScraperWithError RecipeIngredient
    ingredient = do
        amount <- text $ "span" @: [hasClass "wprm-recipe-ingredient-amount"]
        food' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-name"]
        note' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-notes"]
        origText' <- text $ "li" @: [hasClass "wprm-recipe-ingredient"]

        return
            $ case readQuantity amount of
                Nothing ->
                    RecipeIngredient
                        { quantity = Nothing
                        , unit = Nothing
                        , food = amount ++ " " ++ food'
                        , note = note'
                        , originalText = origText'
                        , title = Nothing
                        }
                (Just parsedAmount) ->
                    RecipeIngredient
                        { quantity = Just parsedAmount
                        , unit = Just unit'
                        , food = food'
                        , note = note'
                        , originalText = origText'
                        , title = Nothing
                        }

    instructions :: ScraperWithError [RecipeInstruction]
    instructions = chroots ("section" @: [hasClass "bb-recipe-steps"]) instruction

    instruction :: ScraperWithError RecipeInstruction
    instruction = do
        text' <- text $ "li" @: [hasClass "recipeInstructions"]
        return
            $ RecipeInstruction
                { instructionText = text'
                , instructionTitle = ""
                }
-}
