let
  # https://github.com/tweag/nixpkgs/tree/ud/update-ghchead-20201001
  rev = "78d9696f5d1524fcf2fd947f012ffc08fcba4dd1";
  sha256 = "146rahqm2rkbz2hl689rqc04wcfdrghbjzxzynbzsc3xi5if59bh";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/tweag/nixpkgs/archive/${rev}.tar.gz";
})

