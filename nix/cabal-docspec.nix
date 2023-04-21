{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "cabal-docspec";

  src = pkgs.fetchurl {
    url = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20230406/cabal-docspec-0.0.0.20230406-x86_64-linux.xz";
    sha256 = "68fa9addd5dc453d533a74a763950499d4593b1297c9a05c3ea5bd1acc04c9dd";
  };

  phases = ["installPhase"];

  installPhase = ''
    mkdir -p $out/bin
    ${pkgs.xz}/bin/xz -d < $src > $out/bin/cabal-docspec
    chmod a+x $out/bin/cabal-docspec
  '';
}
