self: pkgs: {
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellSelf: haskellP: {
      hakyll = haskellP.hakyll.overrideAttrs (oldAttrs: rec {
        allowBroken = true;
        patches = [./deps.patch];
      });
    };
  };
}
