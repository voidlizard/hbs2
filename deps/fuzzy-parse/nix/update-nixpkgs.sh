#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-git
nix-prefetch-git https://github.com/NixOS/nixpkgs.git | tee pkgs.json
