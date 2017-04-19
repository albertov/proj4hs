{ mkDerivation, aeson, base, bytestring, data-default, hspec, lens
, linear, QuickCheck, quickcheck-instances, stdenv
, template-haskell, text, unordered-containers
}:
mkDerivation {
  pname = "proj4hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring data-default hspec lens linear QuickCheck
    template-haskell text unordered-containers
  ];
  testHaskellDepends = [
    base hspec lens QuickCheck quickcheck-instances
  ];
  homepage = "https://github.com/GeoHS/proj4hs";
  description = "Pure Haskell port of proj.4";
  license = stdenv.lib.licenses.bsd3;
}
