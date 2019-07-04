# Revision history for xmonad-aloysius

## 0.1.1.0 -- 2019-07-03

* Replaced rofi launcher with dzen implementation as part of xmonad.hs
* Replaced lemonbar powermenu with dzen and xdotool
  + NOTE: there is a fixme here because xdotool spawns 'too quickly'
* Made powermenu use systemctl instead of raw reboot/poweroff
* Removed submaps until I design my preferred keybindings
* Added back in window rotation keys, was lost without these
* Implemented dynamicProjects
* Implemented navigation2d
* Swapped out mouse1 bind for flexibleManipulate linear
* M-s now searches selected text on 'g':duckduckgo, 'h':hoogle, 'w':wikipedia
* M-S-s should prompt for text entry and search but currently seems broken
* Added a three col mid layout
* Added scratchpad on M-`
* Workspace 6 is now all float


## 0.1.0.0 -- 2019-07-01

* First version. Released on an unsuspecting world.
