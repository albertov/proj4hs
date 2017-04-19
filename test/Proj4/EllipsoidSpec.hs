{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Proj4.EllipsoidSpec (main, spec) where

import Proj4

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Printf (printf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  prop "ellpsRf (ellpsFromARf a rf) = rf" $ \(NonZero a, NonZero rf) ->
    let rf' = ellpsRf (ellpsFromARf a rf) :: Double
    in counterexample (printf "%s /= %s" (show rf) (show rf')) $
        abs (rf - rf') < 1e-12

