module Proj4.Util where

radianToDegree :: Fractional a => a -> a
radianToDegree = (*57.29577951308232088)

degreeToRadian :: Fractional a => a -> a
degreeToRadian = (*0.01745329251994329577)

secToRadian :: Fractional a => a -> a
secToRadian = (*4.84813681109535993589914102357e-6)

