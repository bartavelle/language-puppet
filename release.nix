{ supportedSystems ? [ "x86_64-linux"] }:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let hpkgs = pkgs_x86_64_linux.haskellPackages;
in
{
  language_puppet = pkgs.lib.hydraJob (hpkgs.callPackage ./language-puppet.nix {servant = hpkgs.servant_0_9_1_1; servant-client = hpkgs.servant-client_0_9_1_1;});
}
