{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
module Proj4 (
  module Export
, Proj (..)
) where

import Proj4.Datum as Export
import Proj4.Ellipsoid as Export
import Proj4.Projection as Export
import Proj4.Projections as Export

import Control.Lens hiding (transform)
import Data.Aeson
import Data.Text (Text)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift)

import Linear.V3 as Export (V3(..))

data Proj a where
  Proj :: (Typeable algo, IsProjection (Projection algo) a )
       => Projection algo a -> Proj a

deriving instance Typeable (Proj a)
deriving instance Show a => Show (Proj a)
deriving instance Lift a => Lift (Proj a)

instance Eq (Proj a) where
  Proj a == Proj b = sameProj a b

instance HasForwardBackward Proj where
  forward (Proj p) = forward p
  {-# INLINE forward #-}
  backward (Proj p) = backward p
  {-# INLINE backward #-}

instance HasAxis (Proj a) (V3 Axis) where
  axis = lens (\(Proj p)   -> p^.axis)
              (\(Proj p) v -> Proj  (p & axis .~ v))
  {-# INLINE axis #-}

instance HasFromGreenwich (Proj a) a where
  fromGreenwich = lens (\(Proj p)   -> p^.fromGreenwich)
                       (\(Proj p) v -> Proj  (p & fromGreenwich .~ v))
                      
  {-# INLINE fromGreenwich #-}

instance HasDatum (Proj a) (Maybe (Datum a)) where
  datum = lens (\(Proj p)   -> p^.datum)
               (\(Proj p) v -> Proj  (p & datum .~ v))
                      
  {-# INLINE datum #-}

instance forall a. (
    RealFloat a
  , FromJSON a
  , Show a
  , Lift a
  ) => FromJSON (Proj a) where
  parseJSON = withObject "Proj" $ \o -> do
    mInit <- o .:? "init"
    case (mInit :: Maybe Text) of
      Just _init -> undefined --TBD
      Nothing -> do
        pName <- o .: "proj"
        case (pName :: Text) of
          "longlat" -> Proj <$> parseJSON @(Projection Wgs84 a) (Object o)
          "lonlat"  -> Proj <$> parseJSON @(Projection Wgs84 a) (Object o)
          "latlong" -> Proj <$> parseJSON @(Projection Wgs84 a) (Object o)
          "latlon"  -> Proj <$> parseJSON @(Projection Wgs84 a) (Object o)
          _         -> fail "Unknown \"proj\""

{-# SPECIALIZE
  transform :: Proj Double -> Proj Double -> V3 Double -> V3 Double
 #-}
{-# SPECIALIZE
  transform :: ( IsProjection (Projection src) Double
               , IsProjection (Projection dst) Double
               )
            => Projection src Double -> Projection dst Double 
            -> V3 Double -> V3 Double
 #-}
