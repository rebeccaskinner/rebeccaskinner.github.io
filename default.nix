{ doCheck ? true
}:

let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-06-06";
    url = "https://github.com/nixos/nixpkgs/archive/7e9e1b6351bb4acc71f88d3fc4defa3fa899e121.tar.gz";
    sha256 = "1ga7zkkzksgpvymkblj31m55zdrn1ak2iqnisk177x5mgd9vvcqp";
  }){
    config = { allowBroken = true; };
     overlays = [(import ./nix/hakyll)];
  };

  builder = import ./builder { inherit pkgs; };
  site = pkgs.stdenv.mkDerivation {
    name = "rebeccaskinner.net";
    inherit doCheck;
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      "*.cabal"
      "*.hs"
      ".github"
    ] ./.;
    buildInputs = [ builder pkgs.linkchecker ];
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";
    buildPhase = ''
      ${builder}/bin/rebeccaskinner-net-site build
    '';
    checkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };
in
if pkgs.lib.inNixShell then builder
else { inherit builder site; inherit (pkgs) linkchecker; }
