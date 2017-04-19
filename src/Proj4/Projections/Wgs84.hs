{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Proj4.Projections.Wgs84 (Wgs84, wgs84) where

import Proj4.Projection
import Proj4.Util
import Data.Typeable (Typeable)
import Data.Aeson

data Wgs84 = Wgs84
  deriving (Eq, Show, Lift, Typeable)

instance FromJSON Wgs84 where parseJSON = const (pure Wgs84)

instance  Default Wgs84 where def = Wgs84

wgs84 :: Fractional a => Projection Wgs84 a
wgs84 = def

instance HasForwardBackward (Projection Wgs84) where
  forward  = const radianToDegree
  {-# INLINE forward #-}
  backward = const degreeToRadian
  {-# INLINE backward #-}
