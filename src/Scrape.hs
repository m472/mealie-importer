{-# LANGUAGE OverloadedStrings #-}

module Scrape (
    Scrape.scrape,
    parseRecipe,
) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Char (isNumber)
import Data.List.Extra (dropSuffix, dropWhileEnd, trim)
import Data.Set (fromList, member)
import Data.Tuple.Extra (both)
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
        (Just "eat-this.org") -> scrapeWPRM
        (Just "bettybossi.ch") -> scrapeBettyBossi
        Nothing -> const (Left "could not extract domain")
        _allOtherDomains -> const (Left "homepage not (yet) supported")

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

trimCharsStart :: [Char] -> String -> String
trimCharsStart chrs = dropWhile (`member` charSet)
  where
    charSet = fromList chrs

trimCharsEnd :: [Char] -> String -> String
trimCharsEnd chrs = dropWhileEnd (`member` charSet)
  where
    charSet = fromList chrs

trimChars :: [Char] -> String -> String
trimChars chr = trimCharsStart chr . trimCharsEnd chr

-- | Trim common characters from start and end
trimStuff :: String -> String
trimStuff = trimChars ":;, \t\n\r"

logError :: String -> ScraperWithError a
logError message = do
    currentHtml <- html anySelector
    tell [message ++ " " ++ currentHtml]
    empty

continueWithInfo :: String -> ScraperWithError a -> ScraperWithError a
continueWithInfo message scraper = do
    currentHtml <- html anySelector
    tell [message ++ " " ++ currentHtml]
    scraper

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
        description' <- text ("div" @: [hasClass "wprm-recipe-summary"]) <|> continueWithInfo "could not parse description" (pure "")
        ingredients' <- ingredients <|> logError "could not parse ingredients"
        instructions' <- instructions <|> logError "could not parse instructions"
        return $
            Recipe
                { name = recipeTitle
                , orgURL = Nothing
                , prepTime = prepTime'
                , totalTime = totalTime'
                , cookTime = cookTime'
                , description = description'
                , recipeIngredient = concat $ traceShowId ingredients'
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
        let cleanOrigText = unwords . filter (/= "▢") . map trim . words

        amount <- text $ "span" @: [hasClass "wprm-recipe-ingredient-amount"]
        unit' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-unit"]
        food' <- text $ "span" @: [hasClass "wprm-recipe-ingredient-name"]
        note' <- text ("span" @: [hasClass "wprm-recipe-ingredient-notes"]) <|> pure ""
        origText' <- cleanOrigText <$> text ("li" @: [hasClass "wprm-recipe-ingredient"])
        pos <- position

        let title' = case pos of
                0 -> Just groupName
                _ -> Nothing

        return $
            case readQuantity amount of
                Nothing ->
                    RecipeIngredient
                        { quantity = Nothing
                        , unit = Nothing
                        , food = amount ++ " " ++ unit' ++ " " ++ food'
                        , note = note'
                        , originalText = origText'
                        , title = title'
                        }
                (Just parsedAmount) ->
                    RecipeIngredient
                        { quantity = Just parsedAmount
                        , unit = Just unit'
                        , food = food'
                        , note = note'
                        , originalText = origText'
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
        return $
            RecipeInstruction
                { instructionText = text'
                , instructionTitle = if pos == 0 then groupName else ""
                }

scrapeBettyBossi :: String -> Either Error Recipe
scrapeBettyBossi htmlString = processResult $ scrapeStringOrError htmlString recipe
  where
    recipe :: ScraperWithError Recipe
    recipe = do
        recipeTitle <- text $ "h1" @: [hasClass "title"]
        prepTime' <- trimStuff . dropWhile (/= ':') <$> text ("div" @: map hasClass ["icon-duration", "bb-tag", "bb-icon-tag"])
        description' <- text $ "p" @: [hasClass "description"]
        ingredients' <- ingredients
        instructions' <- instructions
        return $
            Recipe
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
    ingredients = chroots "tr" ingredient

    ingredient :: ScraperWithError RecipeIngredient
    ingredient = do
        let reduceWhitespace = unwords . concatMap words . lines
        amount <- text ("td" @: [hasClass "quantity"])
        note' <- trimStuff <$> text ("span" @: [hasClass "post-ingredient"]) <|> pure ""
        food' <- trimStuff . dropSuffix note' <$> text ("td" @: [hasClass "ingredient"])
        origText' <- reduceWhitespace <$> text anySelector

        let (q, u) = both trim $ span isNumber amount

        return $
            case readQuantity q of
                Nothing ->
                    RecipeIngredient
                        { quantity = Nothing
                        , unit = Nothing
                        , food = reduceWhitespace (amount ++ " " ++ food')
                        , note = note'
                        , originalText = origText'
                        , title = Nothing
                        }
                (Just parsedAmount) ->
                    RecipeIngredient
                        { quantity = Just parsedAmount
                        , unit = Just u
                        , food = food'
                        , note = note'
                        , originalText = origText'
                        , title = Nothing
                        }

    instructions :: ScraperWithError [RecipeInstruction]
    instructions = chroots ("li" @: [hasClass "recipeInstructions"]) instruction

    instruction :: ScraperWithError RecipeInstruction
    instruction = do
        text' <- text anySelector
        return $
            RecipeInstruction
                { instructionText = text'
                , instructionTitle = ""
                }
