module Main where

import Calc
import IOJson
import Data.Aeson
import Data.Map ((!))
import qualified Data.Map as Map

main :: IO ()
main = putStrLn "Hello, Haskell!"

readWFile :: FilePath -> IO ()
readWFile p = do
  mRW <- decodeFileStrict p
  case (mRW :: Maybe FileSig) of
    Nothing -> print "Incorrect Format"
    Just rW -> do
      let (w, r) = makeWorld rW
      let wF = fixedPointCalc r w
      print $ totalGiven "Palolene" wF 
