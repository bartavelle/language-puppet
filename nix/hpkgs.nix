{
  pkgs
, compiler ? "default"
}:

if compiler == "default"
then pkgs.haskellPackages
else pkgs.haskell.packages.${compiler}
