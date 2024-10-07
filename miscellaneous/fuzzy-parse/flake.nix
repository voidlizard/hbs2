{
description = "Haskell cabal package";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";

    # another-simple-haskell-flake.url = "something";

    # some-cabal-pkg.url = "github:example/some-cabal-pkg";
    # some-cabal-pkg.flake = false;
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
    haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      # systems = [ "x86_64-linux" ];

      # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
      name = "fuzzy-parse";

      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
      ];

      # Wether to build hoogle in the default shell
      shellWithHoogle = true;


      ## Optional parameters follow

      # nixpkgs config
      # config = { };

      # Add another haskell flakes as requirements
      # haskellFlakes = [ inputs.another-simple-haskell-flake ];

      # Use this to load other flakes overlays to supplement nixpkgs
      # preOverlays = [ ];

      # Pass either a function or a file
      # preOverlay = ./overlay.nix;

      # Override haskell packages
      # hpPreOverrides = { pkgs }: new: old:
      #   with pkgs.haskell.lib; with haskell-flake-utils.lib;
      #   tunePackages pkgs old {
      #     some-haskellPackages-package = [ dontHaddock ];
      #   } // {
      #     some-cabal-pkg = ((jailbreakUnbreak pkgs) (dontCheck (old.callCabal2nix "some-cabal-pkg" inputs.some-cabal-pkg {})));
      #   };

      # Arguments for callCabal2nix
      # cabal2nixArgs = {pkgs}: {
      # };

      # Maps to the devShell output. Pass in a shell.nix file or function
      # shell = ./shell.nix

      # Additional build intputs of the default shell
      # shellExtBuildInputs = {pkgs}: with pkgs; [
      #   haskellPackages.haskell-language-server
      # ];

      # Wether to build hoogle in the default shell
      # shellWithHoogle = true;

    };
}
