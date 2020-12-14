{ system ? builtins.currentSystem }:

with import ./nixpkgs.nix { inherit system; };

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";

  buildInputs = [
    haskell.compiler.ghcHEAD
    cabal-install
    git
    nix
    stack
    cacert
  ];
}


