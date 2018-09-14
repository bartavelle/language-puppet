path: type:
let
  baseName = baseNameOf (toString path);
in
     type != "symlink"
  && baseName != ".stack-work"
  && baseName != "dist"
  && baseName != "dist-newstyle"
  && baseName != "stack.yaml"
  && baseName != ".git"
  && baseName != "default.nix"
  && baseName != "README.adoc"
