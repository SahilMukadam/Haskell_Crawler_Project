import System.Environment

import Database.SQLite.Simple

import Database
import Fetch

fixPath :: URL -> String -> URL
fixPath url path
    | take 4 path == "http" = path
    | otherwise = url ++ path

-- |The main function provides five different functionalities:
--
--  [@create@] will create the database urls.db
--
--  [@saved@] simply prints all the urls currently stored on the database
--
--  [@unfold@] will download and parse all pages on the database, storing all found links
--
--  [@show url@] will download and display the contents of the given url
--
--  [@crawl url@] will download a given page, parse the links and store on the database  
main = do 
    conn <- dbConnection
    args <- getArgs
    case args of
        ["saved"] -> printURLs conn
        ["unfold"] -> unfoldDB conn
        ["show", url] -> do 
            urlText <- downloadURL url
            print urlText
        ["crawl", url] -> do
            print $ "Crawling " ++ url
            urlText <- downloadURL url
            let urls = [ fixPath url path | path <- getPageLinks urlText ]
            mapM_ print urls
            storeURLs conn urls
        _ -> syntaxError
    close conn

syntaxError = putStrLn 
    "Usage: Crawler command [args]\n\
    \\n\
    \crawl URL        Gather links in URL and store in database\n\
    \show URL         Shows contents of given URL\n\
    \saved            List URLs on database\n\
    \unfold           Crawl each of the saved URLs\n"
