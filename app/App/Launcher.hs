-- | Launchers (using dmenu) for finding windows and launching apps

module App.Launcher where

-- Imports ----------------------------------------------------------------------
import           XMonad

import           XMonad.Util.Dzen
import           XMonad.Util.Font               ( Align(..) )

import           App.Alias
import           Theme.ChosenTheme


-- Definitions ------------------------------------------------------------------

powerMenu :: X ()
powerMenu =
  dzenConfig
      (   font sansserif
      -- >=> xScreen 0
      >=> x 2352
      >=> y 0
      >=> align AlignCenter
      >=> bgColor basebg
      >=> addArgs
            [ "-h"
            , "52"
            , "-w"
            , "212"
            , "-l"
            , "3"
            , "-m"
            , "-e"
            , "onstart=uncollapse,grabkeys;"
            ++ "button3=exit:1;"
            ++ "key_Escape=ungrabkeys,exit;"
            ++ "key_s=exec:"
            ++ suspend
            ++ ";"
            ++ "key_r=exec:systemctl reboot;"
            ++ "key_p=exec:systemctl poweroff;"
            , "-p"
            ]
      )
    $  "^fg("
    ++ base12
    ++ ")-^fg()   Power Menu   ^fg("
    ++ base12
    ++ ")-^fg()\n"
    ++ "    ^fg("
    ++ base14
    ++ ")^ca(1, "
    ++ suspend
    ++ ")+^fg()   Suspend^ca()\n"
    ++ "    ^fg("
    ++ base10
    ++ ")^ca(1, systemctl reboot)+^fg()   Reboot^ca()\n"
    ++ "    ^fg("
    ++ base11
    ++ ")^ca(1, systemctl poweroff)+^fg()   Power off  ^ca()"


appLauncher :: String
appLauncher =
  "dmenu_run -p 'Launch application:  ' "
    ++ "-fn \""
    ++ sansserif'
    ++ "\" "
    ++ "-nb \""
    ++ basebg
    ++ "\" "
    ++ "-nf \""
    ++ basefg
    ++ "\" "
    ++ "-sb \""
    ++ base14
    ++ "\" "
    ++ "-sf \""
    ++ base00
    ++ "\" "
    -- non-standard dmenu options, please see: https://tools.suckless.org/dmenu/patches/
    -- my nixOS overlay (aloysius) includes these patches by default
    ++ "-h 52"  -- height 52
    ++ "-F"     -- fuzzy matching
