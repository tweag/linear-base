let
  rev = "f9b3e78132eabde9d01c27b82c6102c87a2ea5a1";
  sha256 = "00dkynb7p9vy511h9g8b6a4r8j26iwsrn0s3433nhcr4v93q17z1";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})

