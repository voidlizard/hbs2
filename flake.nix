{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/master";
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";

    fixme.url = "github:voidlizard/fixme";
    fixme.inputs.nixpkgs.follows = "nixpkgs";

    suckless-conf.url = "github:voidlizard/suckless-conf";
    suckless-conf.inputs.nixpkgs.follows = "nixpkgs";

    saltine = {
      url = "github:tel/saltine/3d3a54cf46f78b71b4b55653482fb6f4cee6b77d";
      flake = false;
    };

};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:

 haskell-flake-utils.lib.simpleCabalProject2flake {
   inherit self nixpkgs;
   systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
   name = "hbs2";

   haskellFlakes = with inputs; [
     suckless-conf
   ];

   packageNames = [
     "hbs2"
     "hbs2-peer"
     "hbs2-core"
     "hbs2-storage-simple"
     "hbs2-git"
     "hbs2-consensus"
   ];

   packageDirs = {
     "hbs2" = "./hbs2";
     "hbs2-tests" = "./hbs2-tests";
     "hbs2-core" = "./hbs2-core";
     "hbs2-storage-simple" = "./hbs2-storage-simple";
     "hbs2-peer" = "./hbs2-peer";
     "hbs2-consensus" = "./hbs2-consensus";
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
   ];

   shellExtBuildInputs = {pkgs}: with pkgs;  [
#     haskellPackages.haskell-language-server
#     haskellPackages.cbor-tool
     pkg-config
     inputs.hspup.packages.${pkgs.system}.default
     inputs.fixme.packages.${pkgs.system}.default
   ];

 };


}
