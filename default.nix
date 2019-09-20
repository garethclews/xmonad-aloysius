{ compiler ? "ghc864", pkgs ? import <nixpkgs> {} }:

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "aloysius" (builtins.path {path = "/home/aloysius/Projects/haskell/mine/xmonad"; name="xmonad-aloysius";}) {};

in
  {
    xmonad = drv;
    xmonad-shell = haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ cabal-install hlint fd ];
    };
  }

