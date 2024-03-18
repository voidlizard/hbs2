{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/master";
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";

    fixme.url = "git+https://git.hbs2.net/Fujv1Uy4W5d9Z7REEArMxbXSJ8nLLn4dYuvaAs8b86hr";
    fixme.inputs.nixpkgs.follows = "nixpkgs";

    suckless-conf.url = "git+https://git.hbs2.net/JAuk1UJzZfbDGKVazSQU5yYQ3NGfk4gVeZzBCduf5TgQ";
    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";

    db-pipe.url = "git+https://git.hbs2.net/5xrwbTzzweS9yeJQnrrUY9gQJfhJf84pbyHhF2MMmSft";
    db-pipe.inputs.nixpkgs.follows = "nixpkgs";

    saltine = {
      url = "github:tel/saltine/3d3a54cf46f78b71b4b55653482fb6f4cee6b77d";
      flake = false;
    };

};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
  let
    packageNames = [
     "hbs2"
     "hbs2-peer"
     "hbs2-core"
     "hbs2-storage-simple"
     "hbs2-git"
     "hbs2-git-reposync"
     "hbs2-qblf"
     "hbs2-keyman"
     "hbs2-share"
     "hbs21-git"
     "hbs2-fixer"
    ];
  in
 haskell-flake-utils.lib.simpleCabalProject2flake {
   inherit self nixpkgs;
   systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
   name = "hbs2";

   haskellFlakes = with inputs; [
     suckless-conf
     db-pipe
   ];

   inherit packageNames;

   packageDirs = {
     "hbs2"        = "./hbs2";
     "hbs2-tests"  = "./hbs2-tests";
     "hbs2-core"   = "./hbs2-core";
     "hbs2-storage-simple" = "./hbs2-storage-simple";
     "hbs2-peer"   = "./hbs2-peer";
     "hbs2-keyman" = "./hbs2-keyman";
     "hbs2-share"  = "./hbs2-share";
     "hbs2-git"    = "./hbs2-git";
     "hbs21-git"   = "./hbs21-git";
     "hbs2-git-reposync"   = "./hbs2-git-reposync";
     "hbs2-fixer"  = "./hbs2-fixer";
   };

   hpPreOverrides = {pkgs, ...}: final: prev: with pkgs; {
     saltine = prev.callCabal2nix "saltine" inputs.saltine { inherit (pkgs) libsodium; };
   };

   packagePostOverrides = { pkgs }: with pkgs; with haskell.lib; [
    disableExecutableProfiling
    disableLibraryProfiling
    dontBenchmark
    dontCoverage
    dontDistribute
    dontHaddock
    dontHyperlinkSource
    doStrip
    enableDeadCodeElimination
    justStaticExecutables

    dontCheck

    (compose.overrideCabal (drv: {
        preBuild = ''
          export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
        '';
      }))

   ];

   shell = {pkgs, ...}:
      pkgs.haskellPackages.shellFor {
        packages = _: pkgs.lib.attrsets.attrVals packageNames pkgs.haskellPackages;
        # withHoogle = true;
        buildInputs = (
          with pkgs.haskellPackages; ([
            ghcid
            cabal-install
            haskell-language-server
            hoogle
            htags
            text-icu
            magic
            pkgs.icu72
            pkgs.openssl
            weeder
          ])
          ++
          [ pkgs.pkg-config
            inputs.hspup.packages.${pkgs.system}.default
            inputs.fixme.packages.${pkgs.system}.default
          ]
        );

        shellHook = ''
        export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
        '';

      };
 };

}
