{ mkDerivation, base, containers, hspec, hspec-expectations
, megaparsec, stdenv
}:
mkDerivation {
  pname = "hspec-megaparsec";
  version = "2.0.0";
  sha256 = "0c4vb0c2y8yar0jjhh24wkkp1g7pbg2wc8h8nw3avfznbil6zyd8";
  libraryHaskellDepends = [
    base containers hspec-expectations megaparsec
  ];
  testHaskellDepends = [ base hspec hspec-expectations megaparsec ];
  homepage = "https://github.com/mrkkrp/hspec-megaparsec";
  description = "Utility functions for testing Megaparsec parsers with Hspec";
  license = stdenv.lib.licenses.bsd3;
}
