module Comtrade (comtrade
                ) where

import           ComtradeConf
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           ParseData
import           System.IO

comtrade :: FilePath -> IO (ComtradeConf, [Line])
comtrade filename = do
  print filename
  handle <- openFile  (filename ++ ".cfg") ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  let conf = parseConf $ lines contents
      nA = chCntA conf
      nD = chCntD conf
  putStr $ "in comtrade file " ++ filename
  putStr $ ", analog channel number: " ++ show nA
  putStr $ ", digital channel number: " ++ show nD
  putStrLn $ ", data file type: " ++ dataFileType conf
  content <- BL.readFile $ filename ++ ".dat"
  let d = runGet (parseData conf) content
  return (conf, d)

