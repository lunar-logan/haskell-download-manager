{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.WX
import Util
--import DB
import NetIO

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow

import Data.Map


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


createPanels f m = do
	fields <- fetchRecord 
	(ps,m') <- getPanels fields f m
	return (ps, m')


mkWidget [] = do
	return []

mkWidget (p:ps) = do
	rest <- mkWidget ps
	return ([widget p] ++ rest)


getPanels [] frm m = do 
	return ([], m)

getPanels (f:fs) frm m = do
	let (TestField id url name len) = f
	(p,m') <- progressPanel (T.unpack url) frm m
	(ys,m'') <- getPanels fs frm m'
	return ([p] ++ ys, m'')


main :: IO ()
main = start gui


-- creats progressbar panel
progressPanel url frm m = do
	p <- panel frm []
	st <- staticText p [text := url]
	progress <- hgauge p 45 [selection := 7]
	let m' = Data.Map.insert url progress m
	set p [layout := 
		column 0 [widget st, widget progress]
		]
	return (p,m')


redrawAdvances f urlEntryWid addBtnWid = do
	let m = Data.Map.empty
	(ps, m') <- createPanels f m
	w <- mkWidget ps
	putStrLn $ show m'
	let ws = [urlEntryWid, addBtnWid] ++ w
	set f [layout := column 0 ws]


--updateProgress m = do
--	fs <- fetchRecord

--updateProgress' [] m = do
--	return ()

--updateProgress' (f:fs) m = do
--	let (TestField id url name len) = f
--	case Data.Map.lookup url m of 
--		Nothing -> error "inconsistent database state"
--		Just pBar -> 




gui :: IO ()
gui = do 
	f <- frame [text := "Download Manager"]
	urlEntry <- textEntry f []
	addBtn <- button f [text := "Add", on command := close f]

	redrawAdvances f (widget urlEntry) (widget addBtn)
	--ps <- resumeApp f
	--wids <- mkWidget ps
	--let ws = [widget urlEntry, widget addBtn] ++ wids
	----p <- progressPanel "http://notespot.in" f
	----q <- progressPanel "http://notespot.in/dl" f
	----r <- progressPanel "http://dl.kashta.com" f
	--set f [layout := column 0 ws]

    --quit <- button f [text := "Quit", on command := close f]
    --progress <- hgauge f 45 [selection := 7]
    --set progress [selection := 25]
    
    
       --set f [layout := widget quit, widget progress]