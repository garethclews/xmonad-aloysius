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
powerMenu = dzenConfig ( font "Font Awesome:size=14"
                         >=> timeout 5
                         >=> xScreen 1
                         >=> x 2346
                         >=> y 0
                         >=> align AlignCenter
                         >=> bgColor basebg
                         >=> addArgs [ "-h", "52"
                                     , "-w", "214"
                                     ]
                       )
                       $
                       "^fg(" ++ base14 ++ ")^ca(1, " ++ suspend ++ ")\xf04c^ca() \
                       \^p(30)^fg(" ++ base08 ++ ")^ca(1, systemctl reboot)\xf021^ca() \
                       \^p(30)^fg(" ++ base11 ++ ")^ca(1, systemctl poweroff)\xf011^ca()"


appLauncher :: String
-- appLauncher = "rofi -show run"
appLauncher = "dmenu_run -p 'Launch application:  ' "
           ++ "-fn \"" ++ sansserif' ++ "\" "
           ++ "-nb \"" ++ basebg     ++ "\" "
           ++ "-nf \"" ++ base04     ++ "\" "
           ++ "-sb \"" ++ base14     ++ "\" "
           ++ "-sf \"" ++ base00     ++ "\" "
           ++ "-h 52"
