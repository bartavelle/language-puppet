# You can build this repository using Nix by running:
#     $ nix-build
#
# You can also open up this repository inside a Nix shell by running:
#
#     $ nix-shell
#
{ pkgs ? import ./nix/pin.nix {}
, compiler ? "default" }:

with pkgs.haskell.lib;

let
  filter =  path: type:
    type != "symlink" && baseNameOf path != ".stack-work"
                      && baseNameOf path != ".git"
                      && baseNameOf path != "README.adoc";
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = dontHaddock
    ( haskellPackages.callCabal2nix
        "language-puppet"
        (builtins.path { name = "language-puppet"; inherit filter; path = ./.; } )
        { }
    );
in
  if pkgs.lib.inNixShell then drv.env else drv
