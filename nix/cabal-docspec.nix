{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "cabal-docspec";

  src = pkgs.fetchurl {
    url = "https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20211114/cabal-docspec-0.0.0.20211114.xz";
    sha256 = "12lgpv4g5lcv5b37crimbmd95n9w8fx45xf3dgncg7lckq6p0972";
  };

  phases = ["installPhase"];

  installPhase = ''
    mkdir -p $out/bin
    ${pkgs.xz}/bin/xz -d < $src > $out/bin/cabal-docspec
    chmod a+x $out/bin/cabal-docspec
  '';
}
