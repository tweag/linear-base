let
  # https://github.com/tweag/nixpkgs/tree/update-ghchead-20201001
  rev = "60a06c5a2ec88392c1eb5e252367dde4e0ead16c";
  sha256 = "0952kxb7zlw81vwk72dm4cxs01ygqgbxsy0ibqsj7khr7xp115jh";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/utdemir/nixpkgs/archive/${rev}.tar.gz";
})
