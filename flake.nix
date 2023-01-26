{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/896219e5bde6efac72198550454e9dd9b5ed9ac9";
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:

 haskell-flake-utils.lib.simpleCabalProject2flake {
   inherit self nixpkgs;
   systems = [ "x86_64-linux" ];
   name = "hbs2";

   packageNames = [
     "hbs2"
     "hbs2-core"
     "hbs2-storage-simple"
     "hbs2-tests"
   ];

   packageDirs = {
     "hbs2" = "./hbs2";
     "hbs2-tests" = "./hbs2-tests";
     "hbs2-core" = "./hbs2-core";
     "hbs2-storage-simple" = "./hbs2-storage-simple";
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
     haskellPackages.haskell-language-server
     inputs.hspup.packages.${pkgs.system}.default
   ];

 };


}
