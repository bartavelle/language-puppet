# You can build this repository by running:
#   $ nix-build
{
  pkgs ? (import (import ./nix/sources.nix).nixpkgs {})
, compiler ? "default"
}:


let
  filter = import ./nix/filter.nix;
  hpkgs = import ./nix/hpkgs.nix {inherit pkgs compiler;};
  haskellPackages = hpkgs.override {
    overrides = self: super: rec {
      language-puppet = with pkgs.haskell.lib;
        disableLibraryProfiling
        ( self.callCabal2nix
            "language-puppet"
            (builtins.path { name = "language-puppet"; inherit filter; path = ./.; } )
            { }
        );
     };
  };
in

# There is no need to create a static exec or other related artifacts as of now
# This is because the drv is used as a library.
# For now we just return the single drv without wrapping it in a record.
haskellPackages.language-puppet
