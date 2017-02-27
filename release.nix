# You can build this repository using Nix by running:
#
#     $ nix-build -A language-puppet release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A language-puppet.env release.nix

{ supportedSystems ? [ "x86_64-linux"] }:
with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./.nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };
in
{
  language-puppet = dontHaddock (pkgs.haskellPackages.callPackage ./. {});
}
