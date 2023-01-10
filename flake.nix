{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=dff5496b12817e3d019983827c4b7ba7beb96580"; # 2022-05-04
    hrubySrc = {
      url = "github:bartavelle/hruby?ref=v0.3.8.1";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, hrubySrc }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; });
    in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = self: super: {
            hruby = (self.callCabal2nix
              "hruby"
              hrubySrc
              { }
            );
            language-puppet = with prev.haskell.lib;
              dontCheck (disableLibraryProfiling
                (self.callCabal2nix
                  "language-puppet"
                  (prev.lib.cleanSource ./.)
                  { }
                ));
          };
        };
      };

      packages = forAllSystems (system:
        with nixpkgsFor.${system};
        {
          default = haskell.lib.justStaticExecutables haskellPackages.language-puppet;
        }
      );

      devShells = forAllSystems (system:
        {
          default = with nixpkgsFor.${system}; mkShell {
            buildInputs = [
              cabal-install
              ruby
              pkg-config
            ];
          };
        });
    };
}
