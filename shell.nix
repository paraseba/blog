{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [

  hakyll
  ghc-mod hasktags hlint hindent
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "blog-haskell-env-0";
  buildInputs = [ ghc pkgs.emacs ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
