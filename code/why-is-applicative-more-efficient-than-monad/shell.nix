{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  #ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  #ghc = pkgs.profiledHaskellPackages.ghcWithPackages (ps: with ps; [
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [

  criterion
  QuickCheck


  #ghc-mod ghci-ng cabal-helper dash-haskell hasktags hlint structured-haskell-mode stylish-haskell

        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
