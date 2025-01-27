{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

-- |Module containing functions that operate on the database urls.db
module Database where

import Types
import Fetch
import Control.Monad
import Data.List
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

instance FromRow Entry where
    fromRow = Entry <$> field <*> field

instance ToRow Entry where
    toRow (Entry url_ processed_) = toRow (url_, processed_)

-- |Method to create the database urls.db
-- It uses connectSqlite3 from Database.HDBC.Sqlite3
dbConnection :: IO Connection
dbConnection = do
    conn <- open "urls.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS urls (url TEXT, processed BOOL)"
    return conn

urlNotInDB :: Connection -> URL -> IO Bool
urlNotInDB conn url = do
    results <- queryNamed conn "SELECT * FROM urls WHERE url = :url" [":url" := url] :: IO [Entry]
    return (length results == 0)

-- |Method to store a list of urls into the database (table url)
storeURLs :: Connection 
          -> [URL] -- ^ List of URLs to be stored on the database
          -> IO ()
storeURLs _ [] = return ()
storeURLs conn urls = do
    -- removes duplicates or URLs already in database
    urls' <- filterM (urlNotInDB conn) (nub urls)
    let query = "INSERT INTO urls (url, processed) VALUES (?, ?)" :: Query
    executeMany conn query (zip urls' (repeat False))

-- |Method to display all the URLs on the database. It uses getURLs.
printURLs :: Connection -> IO ()
printURLs conn = do
    urls <- getURLs conn
    if length urls > 0 then
        mapM_ print urls
    else
        print "No URLS on database yet"

-- |Method to retrieve all the URLs on the database.
getURLs :: Connection -> IO [URL]
getURLs conn = do
    results <- query_ conn "SELECT * FROM urls" :: IO [Entry]
    return $ map (\entry -> url_ entry) results

-- |Method to retrieve all the URLs on the database.
getUnprocessedURLs :: Connection -> IO [URL]
getUnprocessedURLs conn = do
    let query =  "SELECT * FROM urls WHERE processed = :processed" :: Query
    results <- queryNamed conn query [":processed" := False] :: IO [Entry]
    return $ map (\entry -> url_ entry) results

-- |Method to retrive all the URLs from the database, parse and extract new links
-- from these, and store those links on the database. This method could be made more
-- efficient by remembering which URLs have already been processed, so that next
-- time unfoldDB is called only newly retrieved urls should be processed.
unfoldDB :: Connection -> IO ()
unfoldDB conn = do 
    -- Get unprocessed URLS
    urls <- getUnprocessedURLs conn
    -- Process each of these urls
    process conn urls
    -- Flag them as 'processed'
    mapM_ (\url -> executeNamed conn "UPDATE urls SET processed = :processed WHERE url = :url" [":processed" := True, ":url" := url]) urls

-- |Method to download a given list of URLs, extract new links and store these on the database.
process :: Connection 
        -> [URL]  -- ^ List of URLs to be processed
        -> IO ()
process _ [] = return ()
process conn (x:xs) = do 
    print $ "Processing : " ++ x
    urlContent <- downloadURL x
    storeURLs conn (getPageLinks urlContent)
    process conn xs
