{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz
, nixpkgs ? haskellNixSrc + "/nixpkgs" }:

let
spec = {
    url = "https://github.com/input-output-hk/haskell.nix";
    rev = "794f796350de6711234db805af8a1b80d3509b8c";
    date = "2020-05-07T01:09:47+00:00";
    sha256 = "03ilxganwz7h18x0ghxach75wjzbdy9nh3i2lp15z0nvw8y27lyg";
    fetchSubmodules = false;
  };
  haskell-nix-src = (import <nixpkgs> {}).fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
  # Import the Haskell.nix library,
  pkgs = import <nixpkgs> (import haskell-nix-src {}).nixpkgsArgs;
  # pkgs = import nixpkgs (import haskellNixSrc);

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
