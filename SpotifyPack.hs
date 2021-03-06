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
import Data.Function (on)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

data Song = Song { name :: String, artists :: [String], duration :: Float }
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
  parseJSON (T.Object v) = Song <$> v .: "name"
                                <*> (v .: "artists" >>= mapM (.: "name"))
                                <*> v .: "length"
  parseJSON _ = mzero

-- Helped by http://www.the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
parseResponse :: BSL.ByteString -> Maybe [Song]
parseResponse resp =
    do result <- decode resp
       flip T.parseMaybe result $ \obj ->
           parseJSON =<< (obj .: "tracks")


-- Bin-packing "first-fit" algorithm, https://en.wikipedia.org/wiki/Bin_packing_problem

data Bin a b = Bin { items :: [a], available :: b } deriving Eq

addToBin :: Num b => Bin a b -> b -> a -> Bin a b
addToBin bin cost item = bin { items = items bin ++ [item]
                             , available = available bin - cost }

packItem :: (Num b, Ord b) => [Bin a b] -> b -> b -> a -> [Bin a b]
packItem bins binSize cost item
    | cost > binSize = bins -- The item will never fit a bin
    | otherwise      = tooSmall ++ case rest of
                                     []   -> [packIn (Bin [] binSize)]
                                     b:bs -> packIn b : bs
    where
      (tooSmall, rest) = break ((cost <=) . available) bins
      packIn b = addToBin b cost item

packAll :: (Num b, Ord b) => (a -> b) -> b -> [a] -> [Bin a b]
packAll getCost limit = foldr packNext []
    where packNext item bins = packItem bins limit (getCost item) item

-- Public interface for packing
bestPack :: (Num b, Ord b) => (a -> b) -> b -> [a] -> [a]
bestPack getCost limit things =
    case packAll getCost limit things of
      []   -> []
      bins -> items $ minimumBy (compare `on` available) bins



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
[Song {name = "Happy Christmas (War Is Over)", artists = ["Maroon 5"], duration = 207.08},Song {name = "Devil's Work", artists = ["Miike Snow"], duration = 235.68},Song {name = "Happy", artists = ["Never Shout Never"], duration = 156.12}]
*SpotifyPack> totalDuration <$> results "Happy" 600
598.88
-}


-- Compile with: ghc -main-is SpotifyPack -o SpotifyPack SpotifyPack.hs
main :: IO ()
main = do
  (term:(limit:_)) <- getArgs
  playlist <- results term $ read limit
  print playlist
  print $ totalDuration playlist
