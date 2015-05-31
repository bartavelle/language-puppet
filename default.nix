{ mkDerivation, aeson, ansi-wl-pprint, attoparsec, base
, base16-bytestring, bytestring, case-insensitive, containers
, cryptohash, Diff, directory, either, exceptions, filecache, Glob
, hashable, hruby, hslogger, hslua, hspec, HUnit, lens, lens-aeson
, luautils, mtl, operational, optparse-applicative, parallel-io
, parsec, parsers, pcre-utils, process, regex-pcre-builtin
, scientific, servant, servant-client, split, stdenv, stm
, strict-base-types, temporary, text, time, transformers, unix
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "language-puppet";
  version = "1.1.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson ansi-wl-pprint attoparsec base base16-bytestring bytestring
    case-insensitive containers cryptohash Diff directory either
    exceptions filecache Glob hashable hruby hslogger hslua hspec lens
    lens-aeson luautils mtl operational optparse-applicative
    parallel-io parsec parsers pcre-utils process regex-pcre-builtin
    scientific servant servant-client split stm strict-base-types text
    time transformers unix unordered-containers vector yaml
  ];
  testDepends = [
    ansi-wl-pprint base either Glob hspec HUnit lens parsec parsers
    strict-base-types temporary text unix unordered-containers vector
  ];
  homepage = "http://lpuppet.banquise.net/";
  description = "Tools to parse and evaluate the Puppet DSL";
  license = stdenv.lib.licenses.bsd3;
}
