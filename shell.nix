{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, Cabal, cabal-install, containers, stdenv, X11, xmonad
      , xmonad-contrib, xmonad-extras, dbus, utf8-string
      }:
      mkDerivation {
        pname = "aloysius";
        version = "1.0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base
                                     binary
                                     Cabal
                                     cabal-install
                                     containers
                                     dbus
                                     X11
                                     xmonad
                                     xmonad-contrib
                                     xmonad-extras
                                     utf8-string
                                   ];
        homepage = "https://gitlab.com/karetsu/xmonad-aloysius";
        description = "xmonad config";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
