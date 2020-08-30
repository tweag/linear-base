let
  # 2020-08-31 master
  rev = "6716867eb3e763818000eab04f378b86cadc2894";
  sha256 = "19fnqwpq1rk4iibbgq04j9w72s4n31nlz7m26iqhqd5lh7p8sc42";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
