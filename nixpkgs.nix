let
  # https://github.com/tweag/nixpkgs/tree/update-ghchead-20201001
  rev = "647a878e061b1b63efd2094eb9e546c2a7aa0d58";
  sha256 = "1cfn53nls9kfh4ql8bn94zgvajm93bd390927fi345d943hnph84";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/utdemir/nixpkgs/archive/${rev}.tar.gz";
})
