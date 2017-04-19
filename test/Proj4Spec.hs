{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Proj4Spec (main, spec) where

import Proj4
import Proj4.Datums (getDatum)
import Proj4.Ellipsoids (getEllipsoid)

import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Printf (printf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

  describe "transform" $ do
    prop "transform WGS84 WGS84 = id" $ \(LngLat p) ->
      let p' = transform wgs84 wgs84 p
      in counterexample (printf "%s /= %s" (show p) (show p')) $
          p == p'

  describe "getDatum" $ do
    it "can get existing" $
      getDatum "wgs84"  `shouldSatisfy` isJust @ (Datum Double)
    it "can get existing in mixed-case" $
      getDatum "Wgs84"  `shouldSatisfy` isJust @ (Datum Double)
    it "cannot get non-existing" $
      getDatum "foo"  `shouldSatisfy` isNothing @ (Datum Double)

  describe "getEllipsoid" $ do
    it "can get existing" $
      getEllipsoid "SGS85"  `shouldSatisfy` isJust @ (Ellipsoid Double)
    it "can get existing in mixed-case" $
      getEllipsoid "sgs85"  `shouldSatisfy` isJust @ (Ellipsoid Double)
    it "cannot get non-existing" $
      getEllipsoid "sgs99"  `shouldSatisfy` isNothing @ (Ellipsoid Double)

newtype LngLat = LngLat (V3 Double) deriving Show

instance Arbitrary LngLat where
  arbitrary =
    LngLat <$>
      (V3 <$> choose (-180,180) <*> choose (-90,90) <*> choose (0,1000))
