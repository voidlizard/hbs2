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

};

outputs = { self, nixpkgs, flake-utils, ... }@inputs:
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

    miscellaneous =
      [
      "bytestring-mmap"
      "db-pipe"
      "fuzzy-parse"
      "suckless-conf"
      ];

    jailbreakUnbreak = pkgs: pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

    hpOverridesPre = pkgs: new: old: with pkgs.haskell.lib; {
      scotty = new.callHackage "scotty" "0.21" {};
      skylighting-lucid = new.callHackage "skylighting-lucid" "1.0.4" { };
      wai-app-file-cgi = dontCoverage (dontCheck (jailbreakUnbreak pkgs old.wai-app-file-cgi));
    };

    overrideComposable = pkgs: hpkgs: overrides:
      hpkgs.override (oldAttrs: {
        overrides = pkgs.lib.composeExtensions (oldAttrs.overrides or (_: _: { })) overrides;
      });

    makePkgsFromDir = pkgs: pkgNames: mkPath:
      pkgs.lib.genAttrs pkgNames (name:
        pkgs.haskellPackages.callCabal2nix name "${self}/${mkPath name}" {});

    ourHaskellPackages = pkgs: ({}
      // makePkgsFromDir pkgs topLevelPackages (n: n)
      // makePkgsFromDir pkgs keymanPackages (name: "hbs2-keyman/${name}")
      // makePkgsFromDir pkgs miscellaneous (name: "miscellaneous/${name}")
    );

    overlay = final: prev: {
      haskellPackages = overrideComposable prev prev.haskellPackages
        (new: old:
          hpOverridesPre prev new old
            // ourHaskellPackages final
        );
      };

  in
  { overlays.default = overlay; }
  //
  (flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"]
  (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlay];
      };

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

    makePackages = pkgs:
      pkgs.lib.mapAttrs
        (_name: packagePostOverrides) # we can't apply overrides inside our overlay because it will remove linking info
        (pkgs.lib.getAttrs packageNames (ourHaskellPackages pkgs))
        ;

    packagesDynamic = makePackages pkgs;
    packagesStatic = makePackages pkgs.pkgsStatic;
    in  {
    legacyPackages = pkgs;
    homeManagerModules.default = import ./nix/hm-module.nix self;

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


    devShells.default = pkgs.haskellPackages.shellFor {
      packages = _: builtins.attrValues (ourHaskellPackages pkgs);
      # withHoogle = true;
      buildInputs = (
        with pkgs.haskellPackages; [
          ghc
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
          pkgs.libsodium
          pkgs.file
          pkgs.zlib
          pkgs.bzip2
          inputs.hspup.packages.${pkgs.system}.default
        ]
      );

      shellHook = ''
      export GIT_HASH="${self.rev or self.dirtyRev or "dirty"}"
      '';

    };
  }
 ));

}

