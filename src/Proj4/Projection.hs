{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Proj4.Projection (
  Projection
, IsProjection
, HasForwardBackward (..)
, HasDatum (..)
, HasAxis (..)
, HasFromGreenwich (..)
, Axis (..)
, transform
, sameProj

, V3(..)
, Lift
, Default(def)
) where

import Proj4.Datum
import Proj4.Datums (wgs84)
import Proj4.Util (degreeToRadian)

import Data.Aeson
import Control.Applicative (liftA2)
import Control.Lens hiding (transform)
import Data.Default (Default(def))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, eqT, (:~:)(..))
import Linear.V3
import Language.Haskell.TH.Syntax (Lift)

{-
data Units
  = Kilometer
  | Meter
  | Decimeter
  | Centimeter
  | Millimeter
  | NauticalMile
  | Inch
  | Foot
  | Yard
  | Mile
  | Fathom
  | Chain
  | Link
  | UsInch
  | UsFoot
  | UsYard
  | UsMile
  | UsChain
  | InYard
  | InFoot
  | InChain
  deriving (Eq, Ord, Enum, Show)

unitsToMeter :: Fractional a => Units -> a
unitsToMeter Kilometer = 1000
unitsToMeter Meter = 1
unitsToMeter Decimeter = 1/10
unitsToMeter Centimeter = 1/100
unitsToMeter Millimeter = 1/1000
unitsToMeter NauticalMile = 1852.0
          in 0.0254               International Inch
          ft 0.3048               International Foot
          yd 0.9144               International Yard
          mi 1609.344             International Statute Mile
        fath 1.8288               International Fathom
          ch 20.1168              International Chain
        link 0.201168             International Link
       us-in 1./39.37             U.S. Surveyor's Inch
       us-ft 0.304800609601219    U.S. Surveyor's Foot
       us-yd 0.914401828803658    U.S. Surveyor's Yard
       us-ch 20.11684023368047    U.S. Surveyor's Chain
       us-mi 1609.347218694437    U.S. Surveyor's Statute Mile
      ind-yd 0.91439523           Indian Yard
      ind-ft 0.30479841           Indian Foot
      ind-ch 20.11669506          Indian Chain
-}

data Axis = West | South | East | North | Up | Down
  deriving (Eq, Enum, Ord, Show, Lift)

instance {-# OVERLAPS #-} FromJSON (V3 Axis) where
  parseJSON = withText "Axis" $ \(T.unpack . T.toLower -> s) ->
    case s of
      [ew,ns,ud] ->
        V3 <$> ax [('e',East) , ('w', West )] ew
           <*> ax [('n',North), ('s', South)] ns
           <*> ax [('u',Up)   , ('d', Down )] ud
      _       -> fail "Invalid axis format"
      where ax [] _ = fail "Invalid axis format"
            ax ((x,v):xs) c | x==c      = pure v
                            | otherwise = ax xs c

deriving instance Lift (V3 Axis)

data Projection params a = Projection
  { projectionDatum         :: !(Maybe (Datum a))
  , projectionAxis          :: !(V3 Axis)
  , projectionFromGreenwich :: !a
  , projectionParams        :: !params
  } deriving (Eq, Show, Typeable, Lift)
makeFields ''Projection

instance (Fractional a, FromJSON a, FromJSON params, Default params)
  => FromJSON (Projection params a) where
  parseJSON = withObject "Projection" $ \o ->
    Projection
      <$> parseDatum o
      <*> o .:? "axis" .!= (d^.axis)
      <*> parseFromGreenwich o
      <*> parseJSON (Object o)
      where
        d :: Projection params a
        d = def
        parseDatum o = do
          nadGrids <- o .:? "nadgrids"
          case nadGrids :: Maybe Text of
            Just "@null" -> pure Nothing
            Just _       -> fail "nadgrids: not implemented"
            Nothing      -> o .:? "datum"
        parseFromGreenwich o =
          --TODO: Parse and lookup prime meridian
          maybe (d^.fromGreenwich) degreeToRadian <$> o .:? "from_greenwich"

instance (Default params, Fractional a) => Default (Projection params a) where
  def = Projection
    { projectionDatum         = Nothing
    , projectionAxis          = V3 East North Up
    , projectionFromGreenwich = 0
    , projectionParams        = def
    }

class HasForwardBackward proj where
  -- | Project a vector 'v a' in WGS84 coordinates
  --   to a vector in 'proj' coordinates.
  --   Points outside domain produce points which satisfy
  --   'any isNaN p == True'
  forward  :: RealFloat a => proj a -> V3 a -> V3 a

  -- | Project a vector 'v a' in 'proj' coordinates
  --   back to a vector in 'WGS84' coordinates
  --   Points outside domain produce points which satisfy
  --   'any isNaN p == True'
  backward :: RealFloat a  => proj a -> V3 a -> V3 a

type IsProjection proj a =
  ( RealFloat a
  , HasForwardBackward proj
  , Typeable           proj
  , FromJSON          (proj a)
  , Show              (proj a)
  , Lift              (proj a)
  , Eq                (proj a)
  , HasFromGreenwich  (proj a) a
  , HasAxis           (proj a) (V3 Axis)
  , HasDatum          (proj a) (Maybe (Datum a))
  )


sameProj
  :: forall src dst a. (IsProjection src a, IsProjection dst a)
  => src a -> dst a -> Bool
sameProj src dst =
  case eqT of
    Just (Refl :: src :~: dst) -> src==dst
    _                          -> False

transform
  :: forall src dst a. (IsProjection src a, IsProjection dst a)
  => src a -> dst a -> V3 a -> V3 a
transform src dst
  | sameProj src dst = id
  | otherwise
  = adjustAxis (dst^.axis)
  . forward dst
  . (_x -~ dst^.fromGreenwich)
  . datumTransform (src^.datum) (dst^.datum)
  . (_x +~ src^.fromGreenwich)
  . backward src
  . adjustAxis (src^.axis)
{-# INLINEABLE transform #-}



adjustAxis :: Num a => V3 Axis -> V3 a -> V3 a
adjustAxis = liftA2 adjust where
  adjust East  = id
  adjust West  = negate
  adjust North = id
  adjust South = negate
  adjust Up    = id
  adjust Down  = negate
{-# INLINE adjustAxis #-}
