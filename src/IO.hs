{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module IO where

import Calc (BillMap, World, CatMap, District(..))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

-- Input Scheme

data FileSig = FileSig
  { bills :: [SSIRR]
  , districts :: [DistrictIO]
  } deriving (Generic, Show)

data GenR a = GenR
  { n :: String
  , v :: a
  } deriving (Generic, Show)

instance ToJSON (GenR Integer)

instance ToJSON (GenR [SIR])

type SIR = GenR Integer

type SSIRR = GenR [SIR]

data DistrictIO = DistrictIO
  { name :: String
  , availableFunds :: Integer
  , categoryDefaultFunding :: [SIR]
  , billSpecificFunding :: [SSIRR]
  , caps :: [SIR]
  } deriving (Generic, Show)

instance ToJSON DistrictIO

toMapSIR :: SIR -> CatMap
toMapSIR x = Map.singleton (n x) (v x)

toMapSSIRR :: SSIRR -> BillMap
toMapSSIRR x = Map.singleton (n x) (foldr (Map.union . toMapSIR) Map.empty $ v x)

toMapDistrict :: BillMap -> DistrictIO -> World 
toMapDistrict f d = Map.singleton (name d) $
  District
   (Map.union specific def)
   (foldr (Map.union . toMapSIR) Map.empty $ caps d)
   (availableFunds d)
   Map.empty
   []
  where
    cDFM = foldr (Map.union . toMapSIR) Map.empty . categoryDefaultFunding $ d
    specific = foldr (Map.union . toMapSSIRR) Map.empty . billSpecificFunding $ d
    def = Map.mapWithKey (\cat -> Map.map (const $ cDFM ! cat)) f

makeWorld :: FileSig -> (World, BillMap)
makeWorld (FileSig fs ds) = (w, r)
  where
    r = foldr (Map.union . toMapSSIRR) Map.empty fs
    w = foldr (Map.union . toMapDistrict r) Map.empty ds







