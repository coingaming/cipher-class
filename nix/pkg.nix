{ mkDerivation, base, bytestring, cryptonite, esqueleto, extra
, hpack, hspec, hspec-wai, stdenv, text, universum, unliftio
}:
mkDerivation {
  pname = "cipher-class";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    base bytestring cryptonite esqueleto extra hspec hspec-wai text
    universum unliftio
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring cryptonite esqueleto extra hspec hspec-wai text
    universum unliftio
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cipher-class#readme";
  license = stdenv.lib.licenses.bsd3;
}
