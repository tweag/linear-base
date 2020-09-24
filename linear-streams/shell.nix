with import ./nixpkgs.nix {};

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";

  buildInputs = [
    haskell.compiler.ghcHEAD
    git
    nix
    stack
    cacert
  ];
}


