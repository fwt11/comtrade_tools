{-# LANGUAGE DeriveGeneric #-}

module ComtradeToJSON where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Comtrade
import ComtradeConf
import ParseData
import Data.Int
import Data.Matrix
import Data.ByteString.Lazy.Internal (ByteString)

--comToJSON :: ComtradeConf -> [Line] -> ByteString
--comToJSON conf d = encode (toComtrade conf d)

comToJSON :: ComtradeConf -> [Line] -> ByteString
comToJSON  conf d= encode $ toComtrade conf d

toComtrade :: ComtradeConf -> [Line] -> ComtradeData
toComtrade conf d = ComtradeData { nums = getNumColumns d
                                 , timestamps = getTimestampColumns d
                                 , channels = achs ++ dchs
                                 }
  where acols = getAnalogColumns d
        dcols = getDigitalColumns d 
        adata = convertAll conf acols
        ddata = convertDCh dcols
        achs = zipWith (\c d -> ComtradeChannel {channelName = ch_id c, channelData = d}) (achannels conf) adata
        dchs = zipWith (\c d -> ComtradeChannel {channelName = dch_id c, channelData = d}) (dchannels conf) ddata
  
convertAll :: ComtradeConf -> [[Int16]] -> [[Double]]
convertAll comtradeConf = zipWith convertData aChannelConfs
  where aChannelConfs = achannels comtradeConf

convertData :: AChannel -> [Int16] -> [Double]
convertData channelConf analogInts = map restoreData analogInts
  where restoreData  analogInt = a channelConf * fromIntegral analogInt + b channelConf

column :: [[Int16]] -> Int -> [Int16]
column l n = map (!! n) l


getNumColumns :: [Line] -> [Int32]
getNumColumns  = map num

getTimestampColumns :: [Line] -> [Int32]
getTimestampColumns  = map timestamp

getAnalogColumns :: [Line] -> [[Int16]]
getAnalogColumns  = toLists . transpose . fromLists . map (analogPoints) 

getDigitalColumns :: [Line] -> [[Bool]]
getDigitalColumns  = toLists . transpose . fromLists . map (digitalPoints)

boolToInt :: Bool -> Double
boolToInt b
    | b == True = 1
    | otherwise = 0

convertDCh = map (map boolToInt)

data ComtradeData = ComtradeData { nums :: [Int32]
                                 , timestamps :: [Int32]
                                 , channels :: [ComtradeChannel]
                                 } deriving (Generic, Show)

data ComtradeChannel = ComtradeChannel { channelName :: String
                                       , channelData :: [Double]
                                       } deriving (Generic, Show)

instance ToJSON ComtradeChannel
instance ToJSON ComtradeData
