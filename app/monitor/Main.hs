module Main where

import           Comtrade
import           ComtradeToJSON
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import           Lib
import           System.Environment
import           MergeComtrade
import           System.FSNotify
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Data.List as L
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, unless, when)

main :: IO ()
main = do
    lock <- doesFileExist "lock"
    when lock $
        removeFile "lock"
    contents <- readFile "merge.conf"
    let dirs = lines contents
    withManager $ \mgr -> do
        watchTree
          mgr           
          "."           
          (checkDir dirs) 
          (doTheWork dirs) 
    
        forever $ threadDelay 1000000
    
checkDir dirs event = (takeBaseName $ takeDirectory $ eventPath event) `elem` dirs
    
doTheWork :: [FilePath] -> Event -> IO ()
doTheWork dirs event = do 
    let evTime = eventTime event
    let path = eventPath event
    print event
    locked <- doesFileExist "lock"
    unless locked $ do
        writeFile "lock" "lock"
        threadDelay 3000000
        fileAndTimes <- mapM getLastestFileTime dirs
        mergeComtrade $ map dropExtension $ map fst fileAndTimes
        removeFile "lock"
    
getLastestFileTime dir = do
    fLists <- listDirectory dir
    let absPathlists = map (dir </>) fLists
    createdTimes <- mapM getModificationTime absPathlists
    let fileTimes = last $ L.sortOn snd $ zip absPathlists createdTimes
    return fileTimes

