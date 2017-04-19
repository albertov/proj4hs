{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Proj4.Datums where

import Proj4.Datum
import Proj4.Ellipsoid
import Proj4.Ellipsoids (getEllipsoid)
import Proj4.TH (defineDatums)
import Control.Lens
import Data.Aeson

defineDatums "data/Ellipsoid.json" "data/Datum.json"


instance (Fractional a, FromJSON a) => FromJSON (Datum a) where
  parseJSON = withObject "Datum" $ \o -> do
    dName <- o .:? "datum" .!= "wgs84"
    datum :: Datum a <- maybe (fail "Unknown datum") pure (getDatum dName)

    setToWgs84 <- maybe id (set datumToWgs84_) <$> o.:? "towgs84"

    meName <- o .:? "ellps"
    setEllps <- case meName of
      Nothing -> pure id
      Just eName -> do
        ellps :: Ellipsoid a <- maybe (fail "Unknown ellipse") pure (getEllipsoid eName)
        a     <- o .:? "a" .!= ellpsA ellps
        mB     <- o .:? "b"
        mRf    <- o .:? "rf"
        ellps' <- case (mB, mRf) of
          (Nothing, Just rf) ->
            pure $ ellpsFromARf a rf & ellpsName_ .~ ellpsName ellps
          (Just b, Nothing)  -> 
            pure $ ellpsFromAB a b & ellpsName_ .~ ellpsName ellps
          (Nothing, Nothing) ->
            let rf = ellpsRf ellps
            in pure $ ellpsFromARf a rf & ellpsName_ .~ ellpsName ellps
          (Just _, Just _)   -> fail "Both b and rf cannot be present"
        setRa  <- maybe id (set ellpsRa_ ) <$> o .:? "r_a"
        pure (set datumEllps_ (setRa ellps'))
    pure $ datum
      & setEllps
      & setToWgs84
