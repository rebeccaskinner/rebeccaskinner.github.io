{ doCheck ? true
, pkgs ? import <nixpkgs> {}
}:

let
  builder = import ./builder { inherit pkgs; };
  built = pkgs.stdenv.mkDerivation {
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
    buildPhase = ''
      ${builder}/bin/rebeccaskinner-net-site build
'';
    installPhase = ''
      cp -r _site $out
'';
  };
in
if pkgs.lib.inNixShell then builder
else { inherit builder built; inherit (pkgs) linkchecker; }
