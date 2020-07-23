module Lib where

import Comtrade
import ParseData
import Data.List (foldl')
import Prelude
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import PutData
import ComtradeConf
import System.IO

someFunc :: IO ()
someFunc = do
  contents <- readFile "files.conf"
  let filenames = lines contents
  cmtds <- mapM comtrade filenames
  let cdata = map snd cmtds
      cconf = map fst cmtds
      cdn = foldl' mergeData  [] cdata
      cfg = foldl' mergeConf nullConf cconf
  --print $ length cdata
  --print cn
  writeComtradeData "merged.dat" cdn
  writeComtradeConf "merged.cfg" cfg
  return ()

nullConf :: ComtradeConf
nullConf = ComtradeConf {
  achannels = [],
  dchannels = [],
  chCnt = 0,
  chCntA = 0,
  chCntD = 0,
  lineFreq = 0,
  nRates = 0,
  rateL = [],
  timeStamp = [],
  dataFileType = "",
  timeFactor = 0}

writeComtradeData :: FilePath -> [Line] -> IO ()
writeComtradeData filename comtradeData =  BL.writeFile filename $ runPut $ putData comtradeData

mergeLine :: Line -> Line -> Line
mergeLine l1 l2 = Line {num = num l1,
                   timestamp = timestamp l1,
                   analogPoints = (analogPoints l1) ++ (analogPoints l2),
                   digitalPoints = (digitalPoints l1) ++ (digitalPoints l2)
                  }

mergeData :: [Line] -> [Line] -> [Line]
mergeData [] d = d
mergeData d [] = d
mergeData d1 d2 = zipWith mergeLine d1 d2

mergeConf :: ComtradeConf -> ComtradeConf -> ComtradeConf
mergeConf c1 c2 = ComtradeConf {
  achannels = achNew,
  dchannels = dchNew,
  chCnt = chCnt c1 + chCnt c2,
  chCntA = chCntA c1 + chCntA c2,
  chCntD = chCntD c1 + chCntD c2,
  lineFreq = lineFreq c2,
  nRates = nRates c2,
  rateL = rateL c2,
  timeStamp = timeStamp c2,
  dataFileType = dataFileType c2,
  timeFactor = timeFactor c2
  }
 where ach = achannels c1 ++ achannels c2
       dch = dchannels c1 ++ dchannels c2
       achNew = zipWith refreshANum ach [1..(length ach)]
       dchNew = zipWith refreshDNum dch [1..(length dch)]

refreshANum :: AChannel -> Int -> AChannel
refreshANum channel n = channel {an = n}

refreshDNum :: DChannel -> Int -> DChannel
refreshDNum channel n = channel {ddn = n}

showChA :: AChannel -> String
showChA ch = (show $ an ch) ++ "," ++
               (ch_id ch) ++ "," ++
               (ph ch) ++ "," ++
               (ccbm ch) ++ "," ++
               (show $ uu ch) ++ "," ++
               (show $ a ch) ++ "," ++
               (show $ b ch) ++ "," ++
               (show $ skew ch) ++ "," ++
               (show $ min_value ch) ++ "," ++
               (show $ max_value ch) ++ "," ++
               (show $ ps ch)

showChD :: DChannel -> String
showChD ch = (show $ ddn ch) ++
               "," ++
               (dch_id ch) ++
               "," ++
               (dph ch) ++
               "," ++
               (dccbm ch) ++
               "," ++
               (show $ dy ch)

writeComtradeConf :: FilePath -> ComtradeConf -> IO ()
writeComtradeConf filename conf = do
  handle <- openFile filename WriteMode
  hSetEncoding handle utf8
  let cfg = "合并数据,10205#,1999\n" ++ (show $ chCnt conf) ++ "," ++ (show $ chCntA conf) ++ "A," ++
              (show $ chCntD conf) ++ "D\n" ++
              (unlines $ map showChA (achannels conf)) ++
              (unlines $ map showChD (dchannels conf)) ++
              (show $ lineFreq conf) ++ "\n" ++
              (show $ nRates conf) ++ "\n" ++
              (unlines $ rateL conf) ++
              (unlines $ timeStamp conf) ++
              (dataFileType conf) ++ "\n" ++
              (show $ timeFactor conf)
  hPutStrLn handle cfg
  hClose handle
