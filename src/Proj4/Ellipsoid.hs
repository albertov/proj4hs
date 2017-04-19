{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module Proj4.Ellipsoid (
  Ellipsoid (..)
, ellpsFromAB
, ellpsFromARf
, ellpsE
, ellpsEs
, ellpsEp2

, ellpsName_
, ellpsA_
, ellpsB_
, ellpsRf_
, ellpsRa_
) where

import Control.Lens
import Data.Function (on)

import Language.Haskell.TH.Syntax (Lift)

data Ellipsoid a = Ellipsoid
  { ellpsName :: String
  , ellpsA    :: !a
  , ellpsB    :: !a
  , ellpsRf   :: !a   -- Store precalculated rf since some ellipsoids are defined in terms of
                      -- it and conversion loses precision
  , ellpsRa   :: !Bool
  } deriving (Show, Lift)
makeLensesFor [
    ("ellpsName", "ellpsName_")
  , ("ellpsA", "ellpsA_")
  , ("ellpsB", "ellpsB_")
  , ("ellpsRf", "ellpsRf_")
  , ("ellpsRa", "ellpsRa_")
  ] ''Ellipsoid

    
    

instance (Eq a, Fractional a, Ord a) => Eq (Ellipsoid a) where
  (==) a b =
    ((==) `on` ellpsA) a b &&
    -- the tolerance for es is to ensure that GRS80 and WGS84
    -- are considered identical
    ((\x -> (<=5e-12) . abs . subtract x) `on` ellpsEs) a b

ellpsFromAB :: Fractional a => a -> a -> Ellipsoid a
ellpsFromAB a b = Ellipsoid "" a b (deriveEllpsRf a b) False
{-# INLINE CONLIKE ellpsFromAB #-}

ellpsFromARf :: Fractional a => a -> a -> Ellipsoid a
ellpsFromARf a rf = Ellipsoid "" a b rf False where
  b = a * (1 - 1/rf)
{-# INLINE CONLIKE ellpsFromARf #-}

deriveEllpsRf :: Fractional a => a -> a -> a
deriveEllpsRf a b = 1 / (1-b/a)

ellpsEs :: Fractional a => Ellipsoid a -> a
ellpsEs e@Ellipsoid{ellpsA=a, ellpsB=b}
  | ellpsRa e  = 0
  | otherwise    = (a2 - b2) / b2
  where
    a2 = a * a
    b2 = b * b
{-# INLINE ellpsEs #-}

ellpsE :: Floating a => Ellipsoid a -> a
ellpsE e
  | ellpsRa e = 0
  | otherwise    = sqrt (ellpsEs e)
{-# INLINE ellpsE #-}

ellpsEp2 :: Fractional a => Ellipsoid a -> a
ellpsEp2 e@Ellipsoid{ellpsA=a, ellpsB=b, ellpsRa=ra} = (a2 - b2) / b2 where
  a2 = a' * a'
  b2 = b * b
  a' | ra = let es = ellpsEs (e { ellpsRa = False })
            in a * (1 - es * (1/6 + es * (ra4 + es * ra6)))
     | otherwise = a
  ra4 = 17/360
  ra6 = 67/3024
{-# INLINE ellpsEp2 #-}
