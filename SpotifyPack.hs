{-# LANGUAGE OverloadedStrings #-}
module SpotifyPack
where
import qualified Network.URI as URI
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Aeson (FromJSON, parseJSON, decode, (.:))
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data Song = Song { label :: String, duration :: Float }
          deriving (Show, Eq, Ord)

searchURL :: String -> String
searchURL term = "http://ws.spotify.com/search/1/track.json?q=" ++
                 URI.escapeURIString (not . URI.isReserved) term

-- Fetch Songs from Spotify's search API
searchSpotify :: String -> IO (Maybe [Song])
searchSpotify term = do
  response <- simpleHTTP $ getRequest $ searchURL term
  body <- getResponseBody response
  return . parseResponse $ BSL.pack body

instance FromJSON Song where
  parseJSON (T.Object v) = Song <$> v .: "name" <*> v.: "length"
  parseJSON _ = mzero

-- Helped by http://www.the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
parseResponse :: BSL.ByteString -> Maybe [Song]
parseResponse resp =
    do result <- decode resp
       flip T.parseMaybe result $ \obj ->
           parseJSON =<< (obj .: "tracks")
