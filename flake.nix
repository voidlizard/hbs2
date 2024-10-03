{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-flake-utils = { # we don't use haskell-flake-utils directly, but we override input evrywhere
        url = "github:ivanovs-4/haskell-flake-utils/master";
        inputs.flake-utils.follows = "flake-utils";
    };
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

outputs = { self, nixpkgs, flake-utils, ... }@inputs:
  flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"]
  (system:
    let
      packageNames =
        topLevelPackages ++ keymanPackages;

      keymanPackages =
        [
        "hbs2-keyman"
        "hbs2-keyman-direct-lib"
        ];

      topLevelPackages =
        [
        "hbs2"
        "hbs2-peer"
        "hbs2-core"
        "hbs2-storage-simple"
        "hbs2-git"
        "hbs2-git-dashboard"
        "hbs2-qblf"
        "hbs2-fixer"
        "hbs2-cli"
        "hbs2-sync"
        "fixme-new"
        ];

      pkgs = import nixpkgs {
          inherit system;
          overlays = [defaultOverlay];
        };

      defaultOverlay = final: prev:
        (prev.lib.composeManyExtensions
        [
          overlay
          inputs.suckless-conf.overlays.default
          inputs.db-pipe.overlays.default
        ]) final prev;

      packagePostOverrides = pkg: with pkgs.haskell.lib.compose; pkgs.lib.pipe pkg [
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

        (overrideCabal (drv: {
            preBuild = ''
              export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
            '';
          }))
      ];


    jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

    overlay = final: prev: let pkgs = prev; in
    {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: with pkgs.haskell.lib;
          {
            scotty = new.callHackage "scotty" "0.21" { };
            bytestring-mmap = jailbreakUnbreak old.bytestring-mmap; # old.callCabal2nix "bytestring-mmap" inputs.bytestring-mmap {};
            skylighting-lucid = new.callHackage "skylighting-lucid" "1.0.4" { };
            wai-app-file-cgi = dontCoverage (dontCheck (jailbreakUnbreak old.wai-app-file-cgi));
            saltine = old.callCabal2nix "saltine" inputs.saltine { inherit (pkgs) libsodium; };
          }
          // pkgs.lib.genAttrs topLevelPackages (name:
            old.callCabal2nix name "${self}/${name}" {})
          // pkgs.lib.genAttrs keymanPackages (name:
            old.callCabal2nix name "${self}/hbs2-keyman/${name}" {});
        };
      };

    makePackages = pkgs:
      pkgs.lib.mapAttrs
        (_name: packagePostOverrides) # we can't apply overrides inside our overlay because it will remove linking info
        (pkgs.lib.getAttrs packageNames pkgs.haskellPackages);

    # dynamic packages don't work at the moment, because
    # ivanovs-4/bytestring-mmap doesn't compile with ghc 9.4
    # and bytestring-mmap doesn't compire with ghc > 9.6
    packagesDynamic = makePackages pkgs;
    packagesStatic = makePackages pkgs.pkgsStatic;
    in  {
    legacyPackages = pkgs;

    packages =
      packagesDynamic //
      {
        default =
        pkgs.symlinkJoin {
          name = "hbs2-all";
          paths = builtins.attrValues packagesDynamic;
        };
        static =
        pkgs.symlinkJoin {
          name = "hbs2-static";
          paths = builtins.attrValues packagesStatic;
        };
      };

    devShell.default = pkgs.haskellPackages.shellFor {
      packages = _: pkgs.lib.attrsets.attrVals packageNames pkgs.haskellPackages;
      # withHoogle = true;
      buildInputs = (
        with pkgs.haskellPackages; [
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
        ]
        ++
        [ pkgs.pkg-config
          inputs.hspup.packages.${pkgs.system}.default
        ]
      );

      shellHook = ''
      export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
      '';

    };
  }
 );

}
