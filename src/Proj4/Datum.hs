{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Proj4.Datum (
  Datum   (..)
, ToWgs84 (..)
, datumTransform
, datumName_
, datumToWgs84_
, datumEllps_
) where

import           Proj4.Util
import           Proj4.Ellipsoid
import           Control.Lens
import           Data.Aeson
import           Data.Function (on)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Linear.V3
import           Linear.V2
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Lift(lift))


data ToWgs84 a
  = ToWgs84_3 { toWgs84D :: !(V3 a) }
  | ToWgs84_7 { toWgs84D :: !(V3 a)
              , toWgs84R :: !(V3 a)
              , toWgs84M :: !a
              }
  deriving (Eq, Show)

instance Lift a => Lift (ToWgs84 a) where
  lift (ToWgs84_3 (V3 a b c)) =
    [e|ToWgs84_3 (V3 a b c)|]
  lift (ToWgs84_7 (V3 a b c) (V3 d e f) g) =
    [e|ToWgs84_7 (V3 a b c) (V3 d e f) g|]
  

instance (Fractional a, FromJSON a) => FromJSON (ToWgs84 a) where
  parseJSON  = withText "towgs84: expected a string" $ \s -> do
    parts <- mapM lexit ((T.split (==',')) s)
    case parts of
      [a,b,c]         -> pure $ ToWgs84_3 (V3 a b c)
      [a,b,c,d,e,f,g] -> pure $ ToWgs84_7 (V3 a b c) (secToRadian (V3 d e f)) (g / 1e6 + 1)
      _               -> fail "toWgs84 must be 3 or 7 numbers seperated by commas"
    where
      lexit s = either fail pure $ eitherDecode (LBS.fromChunks [T.encodeUtf8 s])

data Datum a = Datum
  { datumName    :: String
  , datumEllps   :: Ellipsoid a
  , datumToWgs84 :: ToWgs84 a
  } deriving (Show, Lift)
makeLensesFor [
    ("datumName", "datumName_")
  , ("datumEllps", "datumEllps_")
  , ("datumToWgs84", "datumToWgs84_")
  ] ''Datum

instance (Eq a, RealFloat a) => Eq (Datum a) where
  a == b = ((==) `on` datumEllps) a b && ((==) `on` datumToWgs84) a b

datumTransform
  :: (RealFloat a, Eq a)
  => Maybe (Datum a) -> Maybe (Datum a) -> V3 a -> V3 a
datumTransform Nothing    _                  = id
datumTransform _          Nothing            = id
datumTransform src        dst     | src==dst = id
datumTransform (Just src) (Just dst)
  = geocentricToGeodetic (datumEllps   dst)
  . geocentricFromWgs84  (datumToWgs84 dst)
  . geocentricToWgs84    (datumToWgs84 src)
  . geodeticToGeocentric (datumEllps   src)
{-# INLINE datumTransform #-}

nan :: Fractional a => a
nan = 0/0

halfPi :: Floating a => a
halfPi = pi/2

geodeticToGeocentric :: RealFloat a => Ellipsoid a -> V3 a -> V3 a
geodeticToGeocentric ellps (V3 x y z) = V3 x' y' z' where
  x'    = (rn + z) * cosY * cosX
  y'    = (rn + z) * cosY * sinX
  z'    = ((rn  * (1 - es)) + z) * sinY
  es    = ellpsEs ellps
  rn    = ellpsA ellps / sqrt (1 - es * sin2Y)
  sinY  = sin yFixed
  sinX  = sin xFixed
  sin2Y = sinY * sinY
  cosX  = cos xFixed
  cosY  = cos yFixed
  xFixed | x > pi    = x - 2*pi
         | otherwise = x
  -- Don't blow up if latitude is just a little bit out of range because it
  -- may just be a rounding issue.
  yFixed | y < (-halfPi) && y > -1.001 * halfPi = -halfPi
         | y >   halfPi  && y <  1.001 * halfPi =  halfPi
         | y < (-halfPi) || y >          halfPi = nan
         | otherwise                            = y

geocentricToGeodetic
  :: RealFloat a
  => Ellipsoid a -> V3 a -> V3 a
geocentricToGeodetic = geocentricToGeodeticWith 30 1e-12

geocentricToGeodeticWith
  :: forall a. RealFloat a
  => Int -> a -> Ellipsoid a -> V3 a -> V3 a
geocentricToGeodeticWith maxIter genau ellps v@(V3 x y z)
  | rr / a < genau = V3 0 (-halfPi) (-b)
  | otherwise      = V3 x' y' z'
  where
    x' | p / a < genau = 0
       | otherwise     = atan2 y x
    rr, p :: a
    rr = sum (v*v)
    p  = let v' = v^._xy in sum (v'*v')
    es = ellpsEs ellps
    a  = ellpsA ellps
    b  = ellpsB ellps
    --------------------------------------------------------------
    -- Following iterative algorithm was developped by
    -- "Institut for Erdmessung", University of Hannover, July 1988.
    -- Internet: www.ife.uni-hannover.de
    -- Iterative computation of CPHI,SPHI and Height.
    -- Iteration of CPHI and SPHI to 10**-12 radian resp.
    -- 2*10**-7 arcsec.
    --------------------------------------------------------------
    (y', z') = (atan (sinPhi / abs cosPhi), h) where
      (V2 sinPhi cosPhi, h) = loop maxIter (sinCosPhi es, z)
      sinCosPhi k =
        let
          rx = 1 / sqrt (1 - k * (2 - k) * st * st)
          ct = z / rr
          st = p / rr
        in V2 (ct * rx) (st * (1 - k) * rx)
      loop !n !(V2 sphi0 cphi0, !z0)
        | n<=0 || sdphi * sdphi <= genau  * genau = (scPhi, newZ)
        | otherwise                               = loop (n-1) (scPhi, newZ)
        where
          !sdphi = sphi * cphi0 - cphi * sphi0
          !scPhi@(V2 sphi cphi) = sinCosPhi rk
          !rk   = es * rn / (rn + newZ)
          !rn   = a / sqrt (1 - es * sphi * sphi)
          !newZ = p * cphi + z0 * sphi - rn * (1 - es * sphi * sphi)
    


geocentricToWgs84 :: Fractional a => ToWgs84 a -> V3 a -> V3 a
geocentricToWgs84 (ToWgs84_3 d      ) = (+ d)
geocentricToWgs84 (ToWgs84_7 d r m) = worker where
  worker p = V3 x' y' z' where
    x' = m * (  p^._x          - p^._y * r^._z + p^._z * r^._y) + d^._x
    y' = m * (  p^._x * r^._z  + p^._y         - p^._z * r^._x) + d^._y
    z' = m * (-(p^._x * r^._y) + p^._y * r^._x - p^._z        ) + d^._z

geocentricFromWgs84 :: Fractional a => ToWgs84 a -> V3 a -> V3 a
geocentricFromWgs84 (ToWgs84_3 d    ) = subtract d
geocentricFromWgs84 (ToWgs84_7 d r m) = worker where
  worker t = V3 x' y' z' where
    p  = (t - d) / pure m
    x' =   p^._x          + p^._y * r^._z - p^._z * r^._y
    y' = -(p^._x * r^._z) + p^._y         + p^._z * r^._x
    z' =   p^._x * r^._y  - p^._y * r^._x + p^._z        
