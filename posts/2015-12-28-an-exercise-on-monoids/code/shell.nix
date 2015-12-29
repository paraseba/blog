{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.pkgs.haskellPackages }:
let
  inherit (nixpkgs) pkgs;
  ghcUtils = nixpkgs.pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    ghc-mod ghci-ng cabal-helper dash-haskell hasktags hlint
    structured-haskell-mode stylish-haskell
    cabal-install
  ]);

  ghc = haskellPackages.ghcWithPackages (ps: with ps; [
    QuickCheck
  ]);

in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ghcUtils ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
