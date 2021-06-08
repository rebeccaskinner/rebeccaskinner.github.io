fixedPoint: pkgs: {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellFixedPoint: haskellPkgs: {
      hakyll = haskellPkgs.hakyll.overrideAttrs (oldAttrs: rec {
        allowBroken = true;
        patches = [./deps.patch];
      });
    };
  };
}
