module Fetch (
    fetch,
    getDomain,
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI (uriAuthority), URIAuth (uriRegName), parseURI)

applyCookiesToRequest :: CookieJar -> Request -> Request
applyCookiesToRequest jar request = request{cookieJar = Just jar}

fetchBettyBossy :: String -> IO L8.ByteString
fetchBettyBossy url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    firstResponse <- httpLbs request manager
    secondResponse <- httpLbs (applyCookiesToRequest (responseCookieJar firstResponse) request) manager
    return $ responseBody secondResponse

fetchGeneric :: String -> IO L8.ByteString
fetchGeneric url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    return $ responseBody response

getDomain :: String -> Maybe String
getDomain urlString = do
    uri <- parseURI urlString
    authority <- uriAuthority uri
    return $ intercalate "." $ reverse $ take 2 $ reverse $ splitOn "." $ uriRegName authority

fetch :: String -> IO L8.ByteString
fetch url =
    let
        fetchMethod =
            ( case getDomain url of
                (Just "bettybossi.ch") -> fetchBettyBossy
                _ -> fetchGeneric
            )
     in
        fetchMethod url
