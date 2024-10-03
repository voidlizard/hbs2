{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/master";
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";
    hspup.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    suckless-conf.url = "git+https://git.hbs2.net/JAuk1UJzZfbDGKVazSQU5yYQ3NGfk4gVeZzBCduf5TgQ";

    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";
    suckless-conf.inputs.fuzzy.follows = "fuzzy";
    suckless-conf.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    db-pipe.url = "git+https://git.hbs2.net/5xrwbTzzweS9yeJQnrrUY9gQJfhJf84pbyHhF2MMmSft";
    db-pipe.inputs.nixpkgs.follows = "nixpkgs";
    db-pipe.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    lsm.url = "git+https://git.hbs2.net/5BCaH95cWsVKBmWaDNLWQr2umxzzT5kqRRKNTm2J15Ls";
    lsm.inputs.nixpkgs.follows = "nixpkgs";
    lsm.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    # fuzzy.url = "git+file:/home/iv/haskell/p2p/hex-offgrid/fuzzy-parse";  # tmp
    fuzzy.url = "git+https://git.hbs2.net/GmcLB9gEPT4tbx9eyQiECwsu8oPyEh6qKEpQDtyBWVPA";
    fuzzy.inputs.nixpkgs.follows = "nixpkgs";
    fuzzy.inputs.haskell-flake-utils.follows = "haskell-flake-utils";

    saltine = {
      url = "github:tel/saltine/3d3a54cf46f78b71b4b55653482fb6f4cee6b77d";
      flake = false;
    };

    bytestring-mmap = {
      url = "github:ivanovs-4/bytestring-mmap";
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
     "hbs2-git-dashboard"
     "hbs2-qblf"
     "hbs2-keyman"
     "hbs2-keyman-direct-lib"
     "hbs2-fixer"
     "hbs2-cli"
     "hbs2-sync"
     "fixme-new"
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
     "hbs2"                   = "./hbs2";
     "hbs2-tests"             = "./hbs2-tests";
     "hbs2-core"              = "./hbs2-core";
     "hbs2-storage-simple"    = "./hbs2-storage-simple";
     "hbs2-peer"              = "./hbs2-peer";
     "hbs2-keyman"            = "./hbs2-keyman/hbs2-keyman";
     "hbs2-keyman-direct-lib" = "./hbs2-keyman/hbs2-keyman-direct-lib";
     "hbs2-git"               = "./hbs2-git";
     "hbs2-git-dashboard"     = "./hbs2-git-dashboard";
     "hbs2-fixer"             = "./hbs2-fixer";
     "hbs2-cli"               = "./hbs2-cli";
     "hbs2-sync"              = "./hbs2-sync";
     "fixme-new"              = "./fixme-new";
   };

   hpPreOverrides = {pkgs, ...}: final: prev: ((with pkgs; {
     saltine = prev.callCabal2nix "saltine" inputs.saltine { inherit (pkgs) libsodium; };
     scotty = final.callHackage "scotty" "0.21" { };
     bytestring-mmap = prev.callCabal2nix "bytestring-mmap" inputs.bytestring-mmap {};
     skylighting-lucid = final.callHackage "skylighting-lucid" "1.0.4" { };
     # wai-app-file-cgi = final.callHackage "wai-app-file-cgi" "3.1.11" { };
     # htags = final.callHackage "htags" "1.0.1" { };
   }) //
   (with haskell-flake-utils.lib;
    with pkgs.haskell.lib;
    let
      donts = [
        (jailbreakUnbreak pkgs)
        # dontBenchmark
        dontCoverage
        dontCheck
      ];
    in tunePackages pkgs prev {
       wai-app-file-cgi = donts;
     }
   ));

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
            # htags
            text-icu
            magic
            pkgs.icu72
            pkgs.openssl
            weeder
          ])
          ++
          [ pkgs.pkg-config
            inputs.hspup.packages.${pkgs.system}.default
          ]
        );

        shellHook = ''
        export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
        '';

      };
 };

}
