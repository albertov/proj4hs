{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Proj4.Projections.Etmerc (Etmerc, etmerc) where

import Proj4.Datum
import Proj4.Projection
import qualified Proj4.Datums as Datums (wgs84)

import Control.Lens

data V6 a = V6 !a !a !a !a !a !a
  deriving (Show, Eq, Lift)


data Etmerc a = Etmerc
  { etmercAxis          :: !(V3 Axis)
  , etmercDatum         :: !(Datum a)
  , etmercFromGreenwich :: !a
  , etmercY0            :: !a
  , etmercX0            :: !a
  , etmercLam0          :: !a
  , etmercPhi0          :: !a
  } deriving (Show, Eq, Lift)
makeFields ''Etmerc

newtype Utm a = Utm (Etmerc a)
  deriving (Show,Eq,Lift)

etmerc :: Fractional a => Etmerc a
etmerc = Etmerc (V3 East North Up) Datums.wgs84 0 0 0 0 0

instance HasForwardBackward Etmerc where
  forward  = const id
  {-# INLINE forward #-}
  backward = const id
  {-# INLINE backward #-}
