{ mkDerivation, base, bytestring, cryptonite, esqueleto, hpack
, hspec, hspec-wai, QuickCheck, quickcheck-instances, stdenv, text
, universum
}:
mkDerivation {
  pname = "cipher-class";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    base bytestring cryptonite esqueleto hspec hspec-wai QuickCheck
    quickcheck-instances text universum
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring cryptonite esqueleto hspec hspec-wai QuickCheck
    quickcheck-instances text universum
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cipher-class#readme";
  license = stdenv.lib.licenses.bsd3;
}
