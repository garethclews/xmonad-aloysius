{ compiler ? "ghc864", pkgs ? import <nixpkgs> {} }:

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "xmonad-aloy" (builtins.path {path = "/home/aloysius/.xmonad"; name="xmaloy";}) {};

in
  {
    swh = drv;
    swh-shell = haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ cabal-install hlint ];
    };
  }

