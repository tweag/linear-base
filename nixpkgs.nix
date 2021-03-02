let
  rev = "e495e2fa72110eaccbbb600fc77bda6e47b906c5";
  sha256 = "1kq5kh4cqchlwgn07277kw9yl95fjcwqqc45kz2x2kf14md56wn4";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})

