module ComtradeConf where

--import System.IO
import Data.List.Split
--import Debug.Trace

data AChannel = AChannel {
                    an :: Int,
                    ch_id :: String,
                    ph :: String,
                    ccbm :: String,
                    uu :: Char,
                    a :: Double,
                    b :: Double,
                    skew :: Double,
                    min_value :: Int,
                    max_value :: Int,
                    primary :: Double,
                    secondary :: Double,
                    ps :: Char}
                deriving (Show)

data DChannel = DChannel {
                    ddn :: Int,
                    dch_id :: String,
                    dph :: String,
                    dccbm :: String,
                    dy :: Int}
                deriving (Show)

data ComtradeConf = ComtradeConf {
                   achannels :: [AChannel],
                   dchannels :: [DChannel],
                   chCnt:: Int,
                   chCntA :: Int,
                   chCntD :: Int,
                   lineFreq :: Int,
                   nRates :: Int,
                   rateL :: [String],
                   timeStamp :: [String],
                   dataFileType :: String,
                   timeFactor :: Int}
                deriving (Show)


parseConf :: [String] -> ComtradeConf
parseConf contents =  ComtradeConf {achannels = (map parseALine $ drop 2 $ take (2 + a_nums) contents),
                                    dchannels = (map parseDLine $ drop (2 + a_nums) $ take (2 + channel_num) contents),
                                    chCnt = channel_num,
                                    chCntA = a_nums,
                                    chCntD = d_nums,
                                    lineFreq = (read $ contents !! (2 + channel_num)) :: Int,
                                    nRates = nrate,
                                    rateL =  drop (channel_num + 4 ) $ take (channel_num + 4 + nrate) contents,
                                    timeStamp = drop (channel_num + 4 + nrate) $ take (channel_num + 4 + nrate + 2) contents,
                                    dataFileType = contents !! (length contents - 2),
                                    timeFactor = read $ last contents :: Int}
                        where [channel_num, a_nums, d_nums] = parseChNum $ contents !! 1
                              nrate = (read $ contents !! (2 + channel_num + 1)) :: Int

parseChNum :: String -> [Int]
parseChNum line = [read $ (head items) :: Int, read $ init (items !! 1) :: Int, read $ init (items !! 2) :: Int]
                  where items = splitOn "," line

parseALine :: String -> AChannel
parseALine line = let field = splitOn "," line
                  in AChannel {
                     an = (read $ field !! 0) :: Int,
                     ch_id = field !! 1,
                     ph = field !! 2,
                     ccbm = field !! 3,
                     uu = field !! 4 !! 0,
                     a = (read $ field !! 5) :: Double,
                     b = (read $ field !! 6) :: Double,
                     skew = (read $ field !! 7) :: Double,
                     min_value = (read $ field !! 8) :: Int,
                     max_value = (read $ field !! 9) :: Int,
                     primary = (read $ field !! 10) :: Double,
                     secondary = (read $ field !! 11) :: Double,
                     ps = field !! 12  !! 0
                   }

parseDLine :: String -> DChannel
parseDLine line = let field = splitOn "," line
                  in DChannel {
                     ddn = (read $ field !! 0) :: Int,
                     dch_id = field !! 1,
                     dph = field !! 2,
                     dccbm = field !! 3,
                     dy = (read $ field !! 4) :: Int}
slice :: Int -> Int -> [a] -> [a]
slice n1 n2 lst = drop n1 $ take n2 lst
