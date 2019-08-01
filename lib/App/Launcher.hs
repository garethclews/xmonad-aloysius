-- | Launchers (using dmenu) for finding windows and launching apps

module App.Launcher where

-- Imports ----------------------------------------------------------------------
import XMonad

import XMonad.Util.Dzen
import XMonad.Util.Font (Align(..))

import App.Alias
import Theme.Nord

-- Definitions ------------------------------------------------------------------
powerMenu :: X ()
powerMenu = dzenConfig ( font "Font Awesome:size=32"
                         >=> timeout 5
                         >=> xScreen 1
                         >=> x 2240
                         >=> y 53
                         >=> align AlignCenter
                         >=> bgColor base00
                         >=> fgColor base04
                         >=> addArgs [ "-h", "120"
                                     , "-w", "320"]
                       )
                       $
                       "^fg(" ++ base14 ++ ")^ca(1, " ++ screensaver ++ ")\xf023^ca() \
                       \^p(65)^fg(" ++ base08 ++ ")^ca(1, systemctl reboot)\xf021^ca() \
                       \^p(65)^fg(" ++ base11 ++ ")^ca(1, systemctl poweroff)\xf011^ca()"

