let
  pkgs = import ./pkgs.nix { inherit config;
                           };
  lib  = pkgs.haskell.lib;
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override { overrides = haskOverrides; };
    };
  };
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner  = "siers";
    repo   = "nix-gitignore";
    rev    = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ignore = gitignore.gitignoreSourceAux ''
    .stack-work
    dist
    dist-newstyle
    .ghc.environment*
    '';
  haskOverrides = new: old:
    let overrides = lib.packagesFromDirectory { directory = ./derivations; } new old;
    in overrides;
in rec {
  inherit pkgs;
  packages = { inherit (pkgs.haskellPackages) fuzzy-parse;
             };
}
