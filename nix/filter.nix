path: type:
let
  baseName = baseNameOf (toString path);
in
     type != "symlink"
  && baseName != ".stack-work"
  && baseName != ".git"
  && baseName != "default.nix"
  && baseName != "README.adoc"
  && baseName != "tests"
