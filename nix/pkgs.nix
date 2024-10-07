import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo  = "nixpkgs";
  inherit (builtins.fromJSON (builtins.readFile ./pkgs.json)) rev sha256;
})
