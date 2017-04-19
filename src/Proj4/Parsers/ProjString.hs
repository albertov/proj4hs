{-# LANGUAGE OverloadedStrings #-}
module Proj4.Parsers.ProjString ( projString, toValue ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Attoparsec.ByteString.Char8 as Atto
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Proj4

projString
  :: (RealFloat a, Show a, Lift a, FromJSON a)
  => ByteString -> Either String (Proj a)
projString = parseEither parseJSON <=< toValue

toValue :: ByteString -> Either String Value
toValue = parseOnly (object <$> parseProj <* endOfInput)

parseProj :: Atto.Parser [Pair]
parseProj = many1 pair
  where
    pair = choice
      [ keyVal num  "a"
      , keyVal num  "b"
      , keyVal num  "rf"
      , keyVal num  "r_a"
      , keyVal num  "R"
      , keyVal num  "k"
      , keyVal num  "e"
      , keyVal num  "lon_0"
      , keyVal num  "lat_0"
      , keyVal num  "lon_1"
      , keyVal num  "lat_1"
      , keyVal num  "lon_wrap"
      , keyVal num  "x_0"
      , keyVal num  "y_0"
      , keyVal num  "x_1"
      , keyVal num  "y_1"
      , keyVal num  "lonc"
      , keyVal num  "zone"
      , keyVal text "proj"
      , keyVal text "datum"
      , keyVal text "ellps"
      , keyVal text "title"
      , keyVal text "towgs84"
      , keyVal text "nadgrids"
      , keyVal text "axis"
      , keyVal text "pm"
      , flag "R_A"
      ]
    keyVal c = keyVal' (char '=' *> c) 
    keyVal' c k = char '+' *> ((.=) <$> (T.decodeUtf8 <$> string k) <*> c) <* skipSpace
    text = String . T.dropAround (==' ') . T.decodeUtf8 <$> (takeWhile1 (/='+') <|> takeByteString)
    flag = keyVal' (pure (Bool True))
    num  = Number <$> scientific


