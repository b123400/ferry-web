{
  extras = hackage:
    { packages = { ferry-web = ./ferry-web.nix; }; };
  resolver = "lts-15.3";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }