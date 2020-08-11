with import ./nixpkgs.nix {};

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";

  buildInputs = [
    (haskell.packages.ghcHEAD.ghcWithPackages (ps: [
      cabal-install
    ]))
    git
    nix
    stack
    cacert
  ];
}


