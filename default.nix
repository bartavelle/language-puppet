# You can build this repository using Nix by running:
#     $ cabal2nix . > language-puppet.nix
#     $ nix-build
#
# You can also open up this repository inside a Nix shell by running:
#
#     $ nix-shell
#
{ compiler ? "default" }:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./.nixpkgs.json);
  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs.rev}.tar.gz";
    inherit (nixpkgs) sha256;
  }) {};
  hlib = pkgs.haskell.lib;
  lib = pkgs.lib;
  filter =  path: type:
              type != "link" && baseNameOf path != ".stack-work";
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  project = hlib.dontHaddock (hlib.overrideCabal
              ( haskellPackages.callPackage ./language-puppet.nix { })
              ( csuper: { src =  lib.cleanSourceWith { inherit filter ; src = lib.cleanSource csuper.src;};})
            );
in

{
inherit project;
}