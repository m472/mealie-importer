{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataModel (
    export,
    Recipe (..),
    RecipeIngredient (..),
)
where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (isNothing)
import GHC.Generics

data Recipe = Recipe
    { name :: String
    , totalTime :: String
    , prepTime :: String
    , cookTime :: String
    , -- , performTime :: String
      description :: String
    , -- , recipeCategory :: [RecipeCategory]
      orgURL :: Maybe String
    , recipeIngredient :: [RecipeIngredient]
    }
    deriving (Show, Generic)

instance ToJSON Recipe where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Recipe

newtype RecipeCategory = RecipeCategory
    { categoryName :: String
    }
    deriving (Show, Generic)

instance ToJSON RecipeCategory where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RecipeCategory

data RecipeIngredient = RecipeIngredient
    { quantity :: Maybe Float
    , unit :: Maybe String
    , food :: String
    , note :: String
    , title :: Maybe String
    , originalText :: String
    }
    deriving (Show, Generic)

instance ToJSON RecipeIngredient where
    toJSON
        ( RecipeIngredient
                { quantity = quantity'
                , unit = unit'
                , food = food'
                , note = note'
                , title = title'
                , originalText = originalText'
                }
            ) =
            object
                [ "quantity" .= quantity'
                , "unit" .= unit'
                , "food" .= food'
                , "note" .= note'
                , "title" .= title'
                , "disableAmount" .= isNothing quantity'
                , "originalText" .= originalText'
                ]

instance FromJSON RecipeIngredient

export :: Recipe -> String
export recipe = toString (encode recipe)
