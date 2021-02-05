{ pkgs ?  import ./nixpkgs.nix {}
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "linear-base";
  buildInputs = [ git zlib ];
  ghc = pkgs.haskell.compiler.ghc901;
  LANG = "en_US.utf8";
}
