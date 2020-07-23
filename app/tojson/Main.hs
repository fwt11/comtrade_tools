module Main where

import           Comtrade
import           ComtradeToJSON
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Lib
import           System.Environment
import MergeComtrade

main :: IO ()
main = do
    fs <- getArgs
    print fs
    let f = head fs
    (conf, d) <- comtrade f
    BL.writeFile  (f ++ ".json") $ comToJSON conf d