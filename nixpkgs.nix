let
  rev = "acd49fab8ece11a89ef8ee49903e1cd7bb50f4b7";
  sha256 = "1zbwifjak4ni71by4xdfp41byz152ll689hqy12pkdwnlspz3g8i";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})

