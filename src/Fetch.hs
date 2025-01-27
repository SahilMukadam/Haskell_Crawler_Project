-- |Module that performs http requests as well as parsing returned html.
module Fetch where

import Data.Maybe
import Data.Either
import Data.Word
import qualified Data.ByteString.Lazy as L
import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String
type HTML = String

downloadURL :: URL -> IO String
downloadURL url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ L8.unpack $ getResponseBody response

-- |Method to split a string at a given keyword
splitKeyword :: String -> String -> (String, String)
splitKeyword _ "" = ([], [])
splitKeyword keyword text
    | take n text == keyword 
      = ([], drop n text)
    | otherwise = (c : before, after)
    where
        n = length keyword
        (c, rest) = (head text, tail text)
        (before, after) = splitKeyword keyword rest

splitAllKeyword :: String -> String -> [String]
splitAllKeyword _ "" = []
splitAllKeyword keyword text = before : splitAllKeyword keyword after
    where
        (before, after) = splitKeyword keyword text

-- |Basic method for extracting all urls of a given text (String)
parseURLs :: HTML              -- ^ String representation of html file
          -> [URL]
parseURLs html = links
    where 
        hrefs = splitAllKeyword "href=\"" html
        links = map (takeWhile (/= '"')) $ tail hrefs

isHTML :: URL -> Bool
isHTML = (==".html"). reverse . (take 5) . reverse 

getPageLinks :: HTML -> [URL]
getPageLinks = filter isHTML . parseURLs
