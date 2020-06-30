{ pkgs ?  import ./nixpkgs.nix {}
, ghc ? pkgs.haskell.compiler.ghcHEAD
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "linear-base";
  buildInputs = [ git gradle zlib ];
  ghc = pkgs.haskell.compiler.ghcHEAD;
  LANG = "en_US.utf8";
}
