# You can build this repository using Nix by running:
#     $ cabal2nix . > default.nix
#     $ nix-build shell.nix
#
# You can also open up this repository inside a Nix shell by running:
#
#     $ nix-shell
#
{ compiler ? "default" }:

let
  _pkgs = import <nixpkgs> { };
  nixpkgs = builtins.fromJSON (builtins.readFile ./.nixpkgs.json);
  src = _pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };
  hlib = pkgs.haskell.lib;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = hlib.dontHaddock (haskellPackages.callPackage ./. {
    megaparsec = haskellPackages.megaparsec_6_1_1;
    hspec-megaparsec = haskellPackages.hspec-megaparsec_1_0_0;
  });
in

if pkgs.lib.inNixShell then drv.env else drv
