let
  # https://github.com/tweag/nixpkgs/tree/ud/ghc9
  rev = "17a21c9466ef77471f5181bc075d7fc2265bcc72";
  sha256 = "12g4jn74az3plzlc8klhbgfjlg2kbxs1238aqymksbxzf2z2n1j3";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/tweag/nixpkgs/archive/${rev}.tar.gz";
})

