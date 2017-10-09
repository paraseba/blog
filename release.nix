let
  config = rec {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          blog-seba =
            pkgs.haskell.lib.overrideCabal
              ( haskellPackagesNew.callPackage ./default.nix {
                }
              )
              ( oldDerivation: {
                  testToolDepends = [  ];
                  #enableSharedExecutables = false;
                }
              );
      };
    };
  };
};

  pkgs = import <nixpkgs> { inherit config; };

in
  {
  blog-seba = pkgs.haskellPackages.blog-seba;

}
