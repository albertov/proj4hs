{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Proj4.TH where

import Proj4.Datum
import Proj4.Ellipsoid

import Control.Monad
import Control.Lens (iforM)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)

data EllipsoidDef a = EllipsoidDef
  { ellipseDefName  :: String
  , ellipseDefA     :: a
  , ellipseDefBorRF :: Either a a
  }

instance FromJSON a => FromJSON (EllipsoidDef a) where
  parseJSON  = withObject "ellipsoidDef: expected an object" $ \o -> do
    name   <- o .:  "ellipseName"
    a      <- o .:  "a"
    mB     <- o .:? "b"
    mRf    <- o .:? "rf"
    bOrRf <- case (mB, mRf) of
      (Nothing, Just rf) -> pure (Right rf)
      (Just b, Nothing)  -> pure (Left b)
      (Nothing, Nothing) -> fail "Either b o rf must be present"
      (Just _, Just _)   -> fail "Both b and rf cannot be present"
    pure EllipsoidDef { ellipseDefName  = name
                      , ellipseDefA     = a
                      , ellipseDefBorRF = bOrRf
                      }

mkEllipsoid :: Fractional a => EllipsoidDef a -> Ellipsoid a
mkEllipsoid ed =
  ( case ellipseDefBorRF ed of
      Left  b  -> ellpsFromAB (ellipseDefA ed) b
      Right rf -> ellpsFromARf (ellipseDefA ed) rf
  ) { ellpsName = ellipseDefName ed}

type EllpsMap a = HM.HashMap Text (Ellipsoid a)
type DatumMap a = HM.HashMap Text (Datum a)

loadEllipsoids
  :: (FromJSON a, Fractional a) => FilePath -> IO (Either String (EllpsMap a))
loadEllipsoids path = fmap (HM.map mkEllipsoid) . eitherDecode <$> LBS.readFile path

loadDatums
  :: (FromJSON a, Fractional a)
  => EllpsMap a -> FilePath -> IO (Either String (DatumMap a))
loadDatums emap path
  = join
  . fmap (sequence . HM.map (mkDatum emap))
  . eitherDecode
  <$> LBS.readFile path

defineEllipsoids :: FilePath -> Q [Dec]
defineEllipsoids file = do
  eMap <- runIO (loadEllipsoids file)
  case eMap of
    Right (emap :: EllpsMap Double) -> do --Dummy Double
      --Dummy Double
      declareFromMap "getEllipsoid" emap
    Left err -> fail ("Could not parse ellipsoids file: " ++ err)

defineDatums :: FilePath -> FilePath -> Q [Dec]
defineDatums ellpsFile file = do
  ellmap <- either fail pure =<< runIO (loadEllipsoids ellpsFile)
  dMap <- runIO (loadDatums ellmap file)
  case dMap of
    Right (dmap :: DatumMap Double) ->
      --Dummy Double
      declareFromMap "getDatum" dmap
    Left err -> fail ("Could not parse datums file: " ++ err)

data DatumDef a = DatumDef
  { datumDefName        :: String
  , datumDefEllipseKey  :: Text
  , datumDefToWGS84     :: ToWgs84 a
  }

declareFromMap :: Lift t => String -> HM.HashMap Text t -> Q [Dec]
declareFromMap getterName dmap = do
  decMap <- iforM dmap $ \(mkName . T.unpack . T.toLower -> name) e ->
    [d|$(pure (VarP name)) = e|]
  a <- newName "a"
  let ellpsDecs = concat (HM.elems decMap)
      funName   = mkName getterName
      funDec    = FunD funName funClauses
      funClauses = map mkClause (HM.keys dmap) ++ [defClause]
      defClause  = Clause [WildP] (NormalB (ConE 'Nothing)) []
      mkClause tName =
        let nameL = T.unpack (T.toLower tName)
            varName = mkName nameL
            gbody =
              ( NormalG (InfixE (Just (VarE 'T.toLower `AppE` VarE a))
                                (VarE '(==))
                                (Just (LitE (StringL nameL))))
              , ConE 'Just `AppE` VarE varName
              )
        in Clause [VarP a] (GuardedB [gbody]) []
  pure (funDec:ellpsDecs)

instance (Fractional a, FromJSON a) => FromJSON (DatumDef a) where
  parseJSON  = withObject "datumDef: expected an object" $ \o -> do
    name       <- o .:  "datumName"
    ellipseKey <- o .:  "ellipse"
    toWgs84    <- o .:? "towgs84" .!= ToWgs84_3 0
    pure DatumDef { datumDefName       = name
                  , datumDefEllipseKey = ellipseKey
                  , datumDefToWGS84    = toWgs84
                  }


mkDatum :: EllpsMap a -> DatumDef a -> Either String (Datum a)
mkDatum emap dd =
  Datum <$> pure (datumDefName dd)
        <*> maybe (Left ("Missing ellipse: " ++ show (datumDefEllipseKey dd))) Right (HM.lookup (datumDefEllipseKey dd) emap)
        <*> pure (datumDefToWGS84 dd)
