module Main where

import Data.Map (Map, (!), fromList)
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
  } deriving (Show)

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

giveFunds :: Map String District -> (Map String District, BillMap)
giveFunds ds = (nDs, b)
  where
    nDs = Map.map actualComputation ds
    b = Map.foldr (Map.unionWith (Map.unionWith (+)) . actual) Map.empty nDs

getBackDistrict :: BillMap -> BillMap -> District -> District
getBackDistrict f b d = District newP newC newA Map.empty
  where
    nPF x y =
      if y <= 0
         then x
         else 0
    sumPlus x y
      | x >= 0 && y >= 0 = x + y
      | x >= 0 = x
      | y >= 0 = y
      | otherwise = 0
    newP = Map.unionWith (Map.unionWith nPF) (pledge d) b 
    bCat = Map.map (Map.foldr sumPlus 0) b
    fundsCat = Map.map (Map.foldr (+) 0) f
    actCat = Map.map (Map.foldr (+) 0) $ actual d
    multC = Map.unionWith (*) bCat actCat
    multDivC = Map.unionWith div multC fundsCat 
    newC = Map.unionWith (+) multDivC $ cap d
    newA = Map.foldr (+) 0 multDivC

getBackWorld :: (Map String District, BillMap) -> BillMap -> (Map String District, BillMap) 
getBackWorld (w, f) r = (newW, newR)
  where
    cmpFR = Map.unionWith (Map.unionWith (-)) f r
    mkPos x
      | x < 0 = abs x
      | otherwise = 0
    newR = Map.map (Map.map mkPos) cmpFR
    newW = Map.map (getBackDistrict f cmpFR) w 


-- Test Data

p1 = fromList 
  [ ("A", fromList [("1", 20), ("2", 25)])
  , ("B", fromList [("3", 30)])]
c1 = fromList [("A", 10), ("B", 20)]
a1 = 30
d1 = District p1 c1 a1 Map.empty 




