module Calc where

import Data.Map (Map, (!), fromList)
import qualified Data.Map as Map

type Category = String

type BillMap = Map Category (Map String Integer)

type CatMap = Map Category Integer

type World = Map String District

data District = District
  { pledge :: BillMap
  , cap :: CatMap
  , available :: Integer
  , actual :: BillMap
  , complete :: [BillMap]
  } deriving (Show)

makeActualPC :: BillMap -> CatMap -> BillMap
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

makeActualA :: Integer -> BillMap -> BillMap
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
modifyPCA d = District newP newC newA act $ complete d
  where
    act = actual $ calcActual d
    newP = Map.unionWith (Map.unionWith (-)) (pledge d) act
    newC = Map.unionWith (-) (cap d) $ Map.map (Map.foldr (+) 0) act
    newA = available d - Map.foldr ((+) . Map.foldr (+) 0) 0 act

actualComputation :: District -> District
actualComputation = modifyPCA . calcActual

giveFunds :: World -> (World, BillMap)
giveFunds ds = (nDs, f)
  where
    nDs = Map.map (appendActualToComplete . actualComputation) ds
    appendActualToComplete d = d {complete = actual d : complete d}
    f = Map.foldr (Map.unionWith (Map.unionWith (+)) . actual) Map.empty nDs

getBackDistrict :: BillMap -> BillMap -> District -> District
getBackDistrict f b d =
  District newP newC newA Map.empty (signedReturnFunds : complete d)
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
    divSafe x y
      | y == 0 = 0
      | otherwise = x `div` y
    onlyPos x
      | x < 0 = 0
      | otherwise = x
    newP = Map.unionWith (Map.unionWith nPF) (pledge d) b
    posB = Map.map (Map.map onlyPos) b
    returnFunds =
      Map.unionWith
        (Map.unionWith divSafe)
        (Map.unionWith (Map.unionWith (*)) (actual d) posB)
        f
    signedReturnFunds = Map.map (Map.map negate) returnFunds
    returnFundsCat = Map.map (Map.foldr (+) 0) returnFunds
    newC = Map.unionWith (+) returnFundsCat $ cap d
    newA = available d + Map.foldr (+) 0 returnFundsCat

getBackWorld ::
     BillMap -> (World, BillMap) -> (World, BillMap)
getBackWorld r (w, f) = (newW, newR)
  where
    cmpFR = Map.unionWith (Map.unionWith (-)) f r
    mkPos x
      | x < 0 = abs x
      | otherwise = 0
    newR = Map.map (Map.map mkPos) cmpFR
    newW = Map.map (getBackDistrict f cmpFR) w

fixedPointCalc :: BillMap -> World -> World
fixedPointCalc r w =
  if newR == r
    then w
    else fixedPointCalc newR newW
  where
    (newW, newR) = getBackWorld r . giveFunds $ w

foldComplete :: [BillMap] -> BillMap
foldComplete = foldr (Map.unionWith (Map.unionWith (+))) Map.empty

totalGiven :: String -> World -> BillMap
totalGiven x d = foldComplete . complete $ d ! x

-- Test Data
p1 =
  fromList [("a", fromList [("1", 20), ("2", 25)]), ("b", fromList [("3", 30)])]

c1 = fromList [("a", 10), ("b", 80)]

a1 = 5

d1 = District p1 c1 a1 Map.empty []

p2 =
  fromList [("a", fromList [("1", 20), ("2", 25)]), ("b", fromList [("3", 30)])]

c2 = fromList [("a", 10), ("b", 0)]

a2 = 30

d2 = District p2 c2 a2 Map.empty []

world = fromList [("x", d1), ("y", d2)]

r = fromList [("a", fromList [("1", 7), ("2", 5)]), ("b", fromList [("3", 70)])]
