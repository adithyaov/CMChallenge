module Main where

import Data.Map (Map, (!))
import qualified Data.Map as Map

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Category = String

type BillMap = Map Category (Map String Int)

type CapMap = Map Category Int

data District = District
  { pledge :: BillMap
  , cap :: CapMap
  , available :: Int
  , actual :: BillMap
  }

makeActualPC :: BillMap -> CapMap -> BillMap
makeActualPC p c = act
  where
    pCatTot = Map.map (Map.foldr (+) 0) p
    pCatF =
      Map.mapWithKey
        (\cat tot ->
           if tot <= c ! cat
             then id
             else (\x -> (x * c ! cat) `div` tot))
        pCatTot
    act = Map.mapWithKey (\cat bM -> Map.map (pCatF ! cat) bM) p

makeActualA :: Int -> BillMap -> BillMap
makeActualA i a = act
  where
    tot = Map.foldr ((+) . Map.foldr (+) 0) 0 a
    act =
      if i >= tot
        then a
        else Map.map (Map.map (\x -> (x * i) `div` tot)) a

calcActual :: District -> District
calcActual d = d {actual = act}
  where
    act = makeActualA (available d) $ makeActualPC (pledge d) (cap d)

modifyPCA :: District -> District
modifyPCA d = District newP newC newA act
  where
    act = actual $ calcActual d
    newP = Map.unionWith (Map.unionWith (-)) (pledge d) act
    newC = Map.unionWith (-) (cap d) $ Map.map (Map.foldr (+) 0) act
    newA = available d - Map.foldr ((+) . Map.foldr (+) 0) 0 act

actualComputation :: District -> District
actualComputation = modifyPCA . calcActual

getFunds :: Map String District -> (Map String District, BillMap)
getFunds ds = (nDs, b)
  where
    nDs = Map.map actualComputation ds
    b = Map.foldr (Map.unionWith (Map.unionWith (+)) . actual) Map.empty nDs

giveBack :: BillMap -> District -> BillMap -> District
giveBack f d b =
  where
    nPF cat bill x y =
      if y <= 0
         then 0
         else x + (y * ((actual d ! cat) ! bill)) `div` ((f ! cat) ! bill)
    sumPlus x y
      | x >= 0 && y >= 0 = x + y
      | x >= 0 = x
      | y >= 0 = y
      | otherwise = 0
    newP = Map.unionWithKey (\cat -> Map.unionWithKey (nPF cat)) (pledge d) b 
    catB = Map.map (Map.foldr sumPlus 0) b
    newC = Map.unionWithKey nCF (cat d) catB

giveBackExtra :: (Map String District, BillMap) -> BillMap -> (Map String District, BillMap) 
giveBackFunds (w, f) r =
  where
    cmpFR = Map.unionWith (Map.unionWith (-)) f r
    Map.map (\d -> give 






