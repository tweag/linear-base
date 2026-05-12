{
  description = "Linear-base";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; inherit overlays; };

        # need to match Stackage LTS version from stack.yaml snapshot
        stack-ghc-version = "ghc9103";

        # Versions used by CI's Cabal-based matrix
        ci-ghc-versions = ["96" "98" "910" "912"];

        cabal-docspec = import ./nix/cabal-docspec.nix { inherit pkgs; };

        buildTools = ghc-ver:
          let hask = pkgs.haskell.packages."${ghc-ver}"; in
          [
            hask.ghc
            pkgs.cabal-install
            stack-wrapped
            cabal-docspec
            pkgs.ormolu # Haskell formatter
          ];

        devTools = ghc-ver:
          let hask = pkgs.haskell.packages."${ghc-ver}"; in
          [
            hask.ghcid # Continuous terminal Haskell compile checker
            hask.haskell-language-server # LSP server for editor
          ];

        mkCIShellFor = ghc-ver: pkgs.mkShell {
          # Set UTF-8 local so that run-tests can parse GHC's unicode output.
          LANG="C.UTF-8";

          buildInputs = buildTools "${ghc-ver}";
        };

        ci-shells = builtins.listToAttrs (map (ver:
          { name = "ci-${ver}"; value = mkCIShellFor "ghc${ver}"; }
          ) ci-ghc-versions);

        mkDevShellFor = ghc-ver: pkgs.mkShell {
          # Set UTF-8 local so that run-tests can parse GHC's unicode output.
          LANG="C.UTF-8";

          buildInputs = buildTools "${ghc-ver}" ++ devTools "${ghc-ver}";

        };

        cabal-dev-shells = builtins.listToAttrs (map (ver:
          { name = "dev-${ver}"; value = mkDevShellFor "ghc${ver}"; }
          ) ci-ghc-versions);

        # See https://docs.haskellstack.org/en/stable/topics/nix_integration/
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        overlays = [
        ];

      in {

        devShells = ci-shells // cabal-dev-shells // {
          default = mkDevShellFor stack-ghc-version;

          ci-stack = mkCIShellFor stack-ghc-version;};
      }
    );
}
