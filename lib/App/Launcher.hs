-- | Launchers (using dmenu) for finding windows and launching apps

module App.Launcher where

-- Imports ----------------------------------------------------------------------
import XMonad

import XMonad.Util.Dzen
import XMonad.Util.Font (Align(..))

import App.Alias
import Theme.ChosenTheme


-- Definitions ------------------------------------------------------------------
powerMenu :: X ()
powerMenu = dzenConfig ( font "Font Awesome:size=12"
                         >=> timeout 5
                         >=> xScreen 1
                         >=> x 2380
                         >=> y 0
                         >=> align AlignCenter
                         >=> bgColor basebg
                         >=> addArgs [ "-h", "52"
                                     , "-w", "200"
                                     ]
                       )
                       $
                       "^fg(" ++ base14 ++ ")^ca(1, " ++ suspend ++ ")\xf04c^ca() \
                       \^p(20)^fg(" ++ base08 ++ ")^ca(1, systemctl reboot)\xf021^ca() \
                       \^p(20)^fg(" ++ base11 ++ ")^ca(1, systemctl poweroff)\xf011^ca()"


appLauncher :: String
appLauncher = "dmenu_run -p 'Launch application:  ' "
           ++ "-fn \"" ++ sansserif' ++ "\" "
           ++ "-nb \"" ++ basebg     ++ "\" "
           ++ "-nf \"" ++ base04     ++ "\" "
           ++ "-sb \"" ++ base14     ++ "\" "
           ++ "-sf \"" ++ base00     ++ "\" "
           -- non-standard dmenu options, please see: https://tools.suckless.org/dmenu/patches/
           -- my nixOS overlay (aloysius) includes these patches by default
           ++ "-h 52"  -- height 52
           ++ "-F"     -- fuzzy matching
