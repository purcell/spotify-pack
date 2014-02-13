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
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

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


-- Bin-packing "first-fit" algorithm, https://en.wikipedia.org/wiki/Bin_packing_problem

data Bin a b = Bin { items :: [a], available :: b } deriving Eq

packItem :: (Num b, Ord b) => [Bin a b] -> b -> b -> a -> [Bin a b]
packItem []     binSize cost item
    | cost < binSize = [ Bin { items = [item], available = binSize - cost } ]
    | otherwise      = []
packItem (b:bs) binSize cost item
    | cost <= available b = (b { items = items b ++ [item], available = available b - cost }) : bs
    | otherwise           = b : packItem bs binSize cost item

packAll :: (Num b, Ord b) => [a] -> (a -> b) -> b -> [Bin a b]
packAll things getCost limit = foldr packNext [] things
    where packNext item bins = packItem bins limit (getCost item) item

-- Public interface for packing
bestPack :: (Num b, Ord b) => (a -> b) -> b -> [a] -> [a]
bestPack getCost limit things = items $ minimumBy compareByAvailable $ packAll things getCost limit
    where compareByAvailable t1 t2 = compare (available t1) (available t2)




-- Tying it all together
packSongs :: Float -> [Song] -> [Song]
packSongs = bestPack duration

results :: String -> Float -> IO [Song]
results term limit = do
  found <- searchSpotify term
  return $ packSongs limit $ fromMaybe [] found

totalDuration :: [Song] -> Float
totalDuration = sum . map duration

{-
*SpotifyPack> results "Happy" 600
[Song {label = "Kiss The Stars", duration = 194.131},Song {label = "Chloe", duration = 198.386},Song {label = "Stay Happy There", duration = 207.026}]
*SpotifyPack> totalDuration `fmap` results "Happy" 600
599.54297
-}


-- Compile with: ghc -main-is SpotifyPack -o SpotifyPack SpotifyPack.hs
main :: IO ()
main = do
  (term:(limit:_)) <- getArgs
  playlist <- results term $ read limit
  print playlist
  print $ totalDuration playlist
