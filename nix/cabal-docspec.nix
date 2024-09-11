{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "cabal-docspec";

  src = pkgs.fetchurl {
    url = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240703/cabal-docspec-0.0.0.20240703-x86_64-linux.xz";
    sha256 = "48bf3b7fd2f7f0caa6162afee57a755be8523e7f467b694900eb420f5f9a7b76";
  };

  phases = ["installPhase"];

  installPhase = ''
    mkdir -p $out/bin
    ${pkgs.xz}/bin/xz -d < $src > $out/bin/cabal-docspec
    chmod a+x $out/bin/cabal-docspec
  '';
}
