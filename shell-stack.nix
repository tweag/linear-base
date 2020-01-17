{ pkgs ?  import ./nixpkgs.nix {}
, ghc ? pkgs.haskell.compiler.ghcLinearTypes20191220
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "linear-base";
  buildInputs = [ git gradle zlib ];
  ghc = pkgs.haskell.compiler.ghcLinearTypes20191220;
  LANG = "en_US.utf8";
}
