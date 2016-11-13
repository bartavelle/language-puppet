{ supportedSystems ? [ "x86_64-linux"] }:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hpkgs = pkgs.haskellPackages;
    servant = hpkgs.servant_0_9_1_1;
    servant-client = hpkgs.servant-client_0_9_1_1;
in
{
  language_puppet = pkgs.lib.hydraJob (dontHaddock (hpkgs.callPackage ./language-puppet.nix {inherit servant servant-client;}));
}
