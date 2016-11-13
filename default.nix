with (import <nixpkgs> {});

pkgs.haskellPackages.callPackage ./language-puppet.nix {servant = pkgs.haskellPackages.servant_0_9_1_1; servant-client = pkgs.haskellPackages.servant-client_0_9_1_1;}
