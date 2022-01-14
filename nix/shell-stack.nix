# Provide Nix support to Stack by expressing system packages required, rather than manually having to install stuff like Zlib
# Inspired by https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc, system ? builtins.currentSystem, sources ? import ./sources.nix, pkgs ? import sources.nixpkgs { inherit system; } }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "linear-base";
  buildInputs = [ ];
}
