{
description = "suckless-cong: sexp based configs";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";

    fuzzy.url =
      # "git+http://git.hbs2/GmcLB9gEPT4tbx9eyQiECwsu8oPyEh6qKEpQDtyBWVPA?ref=sexp-parser&rev=bd3a38904864d5cc333974e7b029412607b46871";
      "git+http://git.hbs2.net/GmcLB9gEPT4tbx9eyQiECwsu8oPyEh6qKEpQDtyBWVPA";

};

outputs = { self, fuzzy, nixpkgs, haskell-flake-utils, ... }@inputs:


    haskell-flake-utils.lib.simpleCabal2flake {

      inherit self nixpkgs;
      # systems = [ "x86_64-linux" ];

      # wtf = import fetcher-flake.out.outPath;
      # project-b = import fuzzy.out.outPath;

      name = "suckless-conf";

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

      haskellFlakes = with inputs; [
        fuzzy
      ];

      # Additional build intputs of the default shell
      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
      ];

      # Wether to build hoogle in the default shell
      # shellWithHoogle = true;

    };
}
