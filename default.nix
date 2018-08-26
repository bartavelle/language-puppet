# You can build this repository by running:
#   $ nix-build
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
  drv = disableLibraryProfiling (dontCheck (dontHaddock
    ( haskellPackages.callCabal2nix
        "language-puppet"
        (builtins.path { name = "language-puppet"; inherit filter; path = ./.; } )
        { }
    )));
in

# There is no need to create a static exec or other related artifacts as of now
# This is because the drv is used as a library.
# For now we just return the single drv without wrapping it in a record.
drv
