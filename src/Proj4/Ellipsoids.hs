{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Proj4.Ellipsoids where
import Proj4.TH (defineEllipsoids)
defineEllipsoids "data/Ellipsoid.json"
