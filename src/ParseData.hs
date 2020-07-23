{-# LANGUAGE Strict #-}


module ParseData where

import           Data.Binary.Get
import           Data.Bits
import           Data.Int
import           ComtradeConf

type AnalogPoint = Int16
type DigitalPoint = Bool

data Line = Line {num       :: Int32,
                  timestamp :: Int32,
                  analogPoints     :: [AnalogPoint],
                  digitalPoints :: [DigitalPoint]
                 }
            deriving (Show)

getAnalogPoint :: Get AnalogPoint
getAnalogPoint = do
  v <- getInt16le
  return $! v

parseAnalogPoints :: Int -> Get [AnalogPoint]
parseAnalogPoints 0 = return []
parseAnalogPoints n = do
  v <- getAnalogPoint
  values <- parseAnalogPoints (n - 1)
  return (v : values)

getDigitalWords :: Int -> Get [Int16]
getDigitalWords 0 = return []
getDigitalWords n = do
  v <- getInt16le
  values <- getDigitalWords (n - 1)
  return (v : values)

parseDigitalPoints :: Int -> Get [DigitalPoint]
parseDigitalPoints n = do
  values <- getDigitalWords $ ceiling (fromIntegral n / 16)
  let ac_values = concat $ map getBits values
      ac_list = take n ac_values
  return ac_list

parseLine :: Int -> Int -> Get Line
parseLine nA nD = do
  num <- getInt32le
  t <- getInt32le
  pointsA <- parseAnalogPoints nA
  pointsD <- parseDigitalPoints nD
  return $ Line {num = num, timestamp = t, analogPoints = pointsA, digitalPoints = pointsD}



parseData :: ComtradeConf -> Get [Line]
parseData conf
    | dataFileType conf == "BINARY"  = parseBinaryData nA nD
    | otherwise = parseAsciiData nA nD
  where nA = chCntA conf
        nD = chCntD conf

parseAsciiData :: Int -> Int -> Get [Line]
parseAsciiData nA nD = undefined

parseBinaryData :: Int -> Int -> Get [Line]
parseBinaryData nA nD = do
  empty <- isEmpty
  if empty
    then return []
    else do
        line <- parseLine nA nD
        ds <- parseBinaryData nA nD
        return (line : ds)



getBits :: Int16 -> [Bool]
getBits a = map (\n -> testBit a n) [0..15]
 


