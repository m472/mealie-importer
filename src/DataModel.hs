{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataModel (
    export,
    Recipe (..),
    RecipeIngredient (..),
    RecipeInstruction (..),
)
where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (isNothing)
import GHC.Generics

data Recipe = Recipe
    { name :: !String
    , totalTime :: !String
    , prepTime :: !String
    , cookTime :: !String
    , -- , performTime :: !String
      description :: !String
    , -- , recipeCategory :: ![RecipeCategory]
      orgURL :: !(Maybe String)
    , recipeIngredient :: ![RecipeIngredient]
    , recipeInstruction :: ![RecipeInstruction]
    }
    deriving (Show, Generic)

newtype RecipeCategory = RecipeCategory
    { categoryName :: String
    }
    deriving (Show, Generic)

data RecipeIngredient = RecipeIngredient
    { quantity :: !(Maybe Float)
    , unit :: !(Maybe String)
    , food :: !String
    , note :: !String
    , title :: !(Maybe String)
    , originalText :: !String
    }
    deriving (Show, Generic)

data RecipeInstruction = RecipeInstruction
    { instructionTitle :: !String
    , instructionText :: !String
    }
    deriving (Show, Generic)

-- Json stuff
instance ToJSON Recipe where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON RecipeCategory where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON RecipeIngredient where
    toJSON
        RecipeIngredient
            { quantity = quantity'
            , unit = unit'
            , food = food'
            , note = note'
            , title = title'
            , originalText = originalText'
            } =
            object
                [ "quantity" .= quantity'
                , "unit" .= unit'
                , "food" .= food'
                , "note" .= note'
                , "title" .= title'
                , "disableAmount" .= isNothing quantity'
                , "originalText" .= originalText'
                ]

instance ToJSON RecipeInstruction where
    toJSON
        RecipeInstruction
            { instructionTitle = title'
            , instructionText = text'
            } =
            object
                [ "title" .= title'
                , "text" .= text'
                ]
export :: Recipe -> String
export recipe = toString (encode recipe)
