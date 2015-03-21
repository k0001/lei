{ }:

with import <nixpkgs> {};

let haskellPackages = pkgs.haskellPackages_ghcjs;
in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: {
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
       };
     };
     inherit (haskellPackages)
                 async
                 bifunctors
                 containers
                 lens
                 mtl
                 pipes
                 pipesConcurrency
                 stm
                 transformers
                 ;
   }
