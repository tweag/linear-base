let
  # 2020-08-22 master
  rev = "d8e671676d91fa9d3ef64b69ba7680d07d79f7a7";
  sha256 = "0bpf3qncb1qxg7dx4rgmwbhbi74lia6jdzjvmg8wvalc738p6wgw";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
