module Main where

import Calc
import IOJson
import Data.Aeson
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    (p:s:o:_) -> parseO o p s
    _ -> print "3 args required (min), src file, dest file and option."
  where
    parseO o p s
      | o == "STEPS" = readWFile STEPS p s
      | o == "FINAL" = readWFile FINAL p s
      | otherwise = print "Option should either be STEPS for steps or FINAL for final"

data Option = FINAL | STEPS

readWFile :: Option -> FilePath -> FilePath -> IO ()
readWFile o p s = do
  mRW <- decodeFileStrict p
  case (mRW :: Maybe FileSig) of
    Nothing -> print "Incorrect Format"
    Just rW -> do
      let (w, r) = makeWorld rW
      let wF = fixedPointCalc r w
      let steps = Map.map complete wF
      let final = Map.map foldComplete steps
      case o of
        FINAL -> BSL.writeFile s (encode final)
        STEPS -> BSL.writeFile s (encode steps)
