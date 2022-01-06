{ nixpkgs ? import <nixpkgs> { }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  pkg = pkgs.haskellPackages.callCabal2nix "aloysius" (builtins.path {
    path = "/home/aloysius/Projects/haskell/xmonad";
    name = "aloysius";
  });

  drv = pkgs.haskellPackages.callPackage pkg { };
in if pkgs.lib.inNixShell then drv.env else drv
