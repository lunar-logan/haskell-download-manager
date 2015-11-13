{-# LANGUAGE OverloadedStrings #-}

import NetIO
import Util

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import qualified Data.ByteString.Lazy as B
import Data.Map
import Data.List
import Data.List.Split
import System.Environment
import Control.Concurrent
import Network.HTTP
import Network.URI
import Network.Stream



-- database handling code
data TestField = TestField Int T.Text T.Text Int deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field

instance SQLite.ToRow TestField where
  toRow (TestField id_ url file_name length) = SQLite.toRow (id_, url, file_name, length)

dbName :: String
dbName = "download.db"


storeRecord :: String -> String -> Int -> IO ()
storeRecord urlString fileName length = do
  conn <- SQLite.open dbName
  SQLite.execute conn "INSERT INTO files (url, file_name, length) VALUES (?,?,?)" (urlString :: String, fileName :: String, length :: Int)
  SQLite.close conn


deleteRecord :: Int -> IO ()
deleteRecord id = do
  conn <- SQLite.open dbName
  SQLite.execute conn "DELETE FROM files WHERE id=?" (SQLite.Only id)
  SQLite.close conn


fetchRecord :: IO [TestField]
fetchRecord = do
  conn <- SQLite.open dbName 
  xs <- SQLite.query_ conn "select * from files" :: IO [TestField]
  --mapM_ callback xs
  SQLite.close conn
  return xs
 -- database handling code ends


--downloadFile :: String -> IO (){-
--downloadFile urlString = do
  --let fileName = getFileName urlString -- extract the filename from the url
  --req <- simpleHTTP (headRequest urlString)-}




notNull :: [a] -> Bool
notNull = not . Data.List.null

-- Extracts the file name from the uri
getFileName :: String -> String
getFileName urlString = 
  last $ Data.List.filter notNull (splitOn "/" urlString)


retryDownloads = do
  records <- fetchRecord -- fetch all the records from the database
  retryDownloads' records


retryDownloads' [] = do
  return ()
retryDownloads' (f:fs) = do
  let (TestField id url name len) = f
  forkIO $ do
    resumeDownload id (T.unpack url) (T.unpack name) len 
  retryDownloads' fs


resumeDownload :: Int -> String -> String -> Int -> IO ()
resumeDownload id urlString fileName contentLength = do
  len <- Util.getFileSize fileName
  case len of 
    Nothing -> error ("invalid file, database maybe corrupted")
    Just l -> if (fromInteger l) >= contentLength then do
      putStrLn "Your download is complete deleting from the database"
      deleteRecord id
      else do
        putStrLn ("Downloading content of the file " ++ fileName)
        downloadContent urlString fileName (fromInteger l) (-1)


-- downloads the file from the web
downloadContent :: String -> String -> Int -> Int -> IO ()
downloadContent urlString fileName fromIndex toIndex = do
  bytes <- NetIO.nioReadHTTP' urlString fromIndex toIndex
  B.appendFile fileName bytes
