{ mkDerivation, base, bytestring, cryptonite, esqueleto
, generic-arbitrary, hpack, hspec, persistent, persistent-template
, QuickCheck, quickcheck-instances, stdenv, text, universum
}:
mkDerivation {
  pname = "cipher-class";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    base bytestring cryptonite esqueleto generic-arbitrary hspec
    persistent persistent-template QuickCheck quickcheck-instances text
    universum
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring cryptonite esqueleto generic-arbitrary hspec
    persistent persistent-template QuickCheck quickcheck-instances text
    universum
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cipher-class#readme";
  license = stdenv.lib.licenses.bsd3;
}
