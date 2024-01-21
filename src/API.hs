{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API (
    getAbout,
    getUnits,
    getFoods,
    getUnit,
    About (..),
    Unit (..),
    Food (..),
) where

import Data.Aeson
import Data.Proxy
import Debug.Trace (traceShowId)
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

type ApiToken = String

data OverviewPage a = OverviewPage
    { page :: !Int
    , per_page :: !Int
    , items :: ![a]
    , total :: !Int
    , total_pages :: !Int
    , next :: !(Maybe String)
    , previous :: !(Maybe String)
    }
    deriving (Show, Generic)

instance (FromJSON a) => FromJSON (OverviewPage a)

data About = About
    { production :: !Bool
    , version :: !String
    , demoStatus :: !Bool
    , allowSignup :: !Bool
    }
    deriving (Show, Generic)

instance FromJSON About

data Unit = Unit
    { name :: !String
    , pluralName :: !(Maybe String)
    , description :: !String
    , fraction :: !Bool
    , abbreviation :: !String
    , useAbbreviation :: !Bool
    , aliases :: ![String]
    , id :: !String
    }
    deriving (Show, Generic)

instance FromJSON Unit

data Food = Food
    { name :: !String
    , pluralName :: !(Maybe String)
    , description :: !String
    , id :: !String
    }
    deriving (Show, Generic)

instance FromJSON Food

type AuthHeader = Header "Authorization" ApiToken

type API =
    "app" :> "about" :> AuthHeader :> Get '[JSON] About
        :<|> "units" :> QueryParam "page" Int :> AuthHeader :> Get '[JSON] (OverviewPage Unit)
        :<|> "units" :> Capture "unit-id" String :> AuthHeader :> Get '[JSON] Unit
        :<|> "foods" :> QueryParam "page" Int :> AuthHeader :> Get '[JSON] (OverviewPage Food)

api :: Proxy API
api = Proxy

about :: Maybe String -> ClientM About
units :: Maybe Int -> Maybe String -> ClientM (OverviewPage Unit)
unit :: String -> Maybe String -> ClientM Unit
foods :: Maybe Int -> Maybe String -> ClientM (OverviewPage Food)
about :<|> units :<|> unit :<|> foods = client api

get :: (Maybe String -> ClientM a) -> ApiToken -> IO a
get f token = do
    manager' <- newManager tlsManagerSettings
    unit' <- runClientM (f $ Just $ "Bearer " ++ token) (mkClientEnv manager' (BaseUrl Https "mealie.graf-hub.com" 443 "api"))
    case unit' of
        Left err -> error ("This did not work " ++ show err)
        Right value -> do
            return value

getAll :: (Maybe Int -> Maybe String -> ClientM (OverviewPage a)) -> ApiToken -> IO [a]
getAll f token = getAll' 1
  where
    getAll' i = do
        overviewPage <- get (f (Just i)) token
        if page overviewPage == total_pages overviewPage
            then return $ items overviewPage
            else (items overviewPage ++) <$> getAll' (i + 1)

getAbout :: ApiToken -> IO About
getAbout = get about

getUnits :: ApiToken -> IO [Unit]
getUnits = getAll units

getUnit :: String -> ApiToken -> IO Unit
getUnit id' = get (unit id')

getFoods :: ApiToken -> IO [Food]
getFoods = getAll foods
