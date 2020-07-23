{-# LANGUAGE Strict #-}

module PutData where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import ParseData
import Data.Bits
import Data.List
import Data.List.Split
import Data.Int


putData :: [Line] -> Put
putData = mapM_ putLine

putLine :: Line -> Put
putLine line = do
  putInt32le $ num line
  putInt32le $ timestamp line
  putAnalogPoints $ analogPoints line
  putDigitalPoints $ digitalPoints line

putAnalogPoints :: [AnalogPoint] -> Put
putAnalogPoints aps = mapM_ putInt16le aps

putDigitalPoints :: [DigitalPoint] -> Put
putDigitalPoints dps =  mapM_ putInt32le (genBits dps)

putComtrade :: [Line] -> Put
putComtrade = mapM_ putLine

getbits :: [DigitalPoint] -> Int32
getbits l = foldl' setBit 0 (elemIndices True l)

getwords :: [DigitalPoint] -> [[DigitalPoint]]
getwords  = chunksOf 32

wordsCount :: [AnalogPoint] -> Int
wordsCount dps = ceiling ((fromIntegral $ length dps) / 32)

genBits :: [DigitalPoint] -> [Int32]
genBits l = map getbits $ getwords l
