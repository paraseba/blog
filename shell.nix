{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [

  hakyll

ghc-mod ghci-ng cabal-helper dash-haskell hasktags hlint structured-haskell-mode stylish-haskell

        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc pkgs.emacs ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
