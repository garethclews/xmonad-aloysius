# [Call Me Al](https://www.youtube.com/watch?v=uq-gYOrU8bA)


**DEPRECATED**: I have moved to wayland so not using this at all.

A multi-themed XMonad configuration by Aloysius.


## Contents

* Overview of recent updates and tasks
* [Previews](#latest-previews)
* [Quick-start](#quick-start)
* [Features](#features)
* [Dependencies](#dependencies)
* [Default applications](#default-applications)
* [Installation instructions](#installation-instructions)
* [Key bindings](#key-bindings)


## Newest addition

Say hello to clickable window decorations!


## Next tasks

- [ ] Move a the non-critical parts of this README into the wiki
- [ ] Sort out the screenshots for the missing themes


## Details

This XMonad configuration is my attempt at making it feel more like a desktop
environment. This means a lot more mod-cons than you typically find in a tiling
WM. You will also need my polybar configuration, available in my nix-overlay, as
well as all of the supporting scripts available
[here](https://gitlab.com/karetsu/scripts).


My personal setup looks like:

- **OS**:       nixOS
- **WM**:       XMonad
- **Terminal**: kitty
- **Launcher**: dmenu2
- **Editor**:   [doom emacs](https://github.com/hlissner/doom-emacs)
- **Browser**:  firefox


See below for how to change these for your preferred options. If the build
instructions do not seem to work on your preference of OS then please raise an
issue. As the build uses a nix-shell then it should "just work".


## Latest Previews

| Main desktop                                 |
| ---                                          |
| ![Screenshot](https://imgur.com/mCqz67s.png) |


| Named scratchpad popup                         |
| ---                                            |
| ![Screenshot](https://i.imgur.com/jJcEoeQ.png) |

| Lock screen                                    |
| ------------------------------------------     |
| ![Screenshot](https://i.imgur.com/KVXTttS.png) |



## Quick-start

In order to access everything once you get into the WM make use of the following keybinds until you find your way around.

| Key Binding                              | Action                             |
|------------------------------------------|------------------------------------|
| <kbd>M</kbd> <kbd>Return</kbd>           | Launch terminal                    |
| <kbd>M</kbd> <kbd>p</kbd>                | Spawn dmenu to launch applications |
| <kbd>M</kbd> <kbd>q</kbd> <kbd>q</kbd>   | Quit XMonad                        |
| <kbd>M</kbd> <kbd>1</kbd>..<kbd>8</kbd>  | Change to workspace 1 thro' 8      |
| <kbd>M</kbd> <kbd>a</kbd> <kbd>q</kbd>   | Close application                  |


## Dependencies

Here are all of the things which this setup needs to work. If you install all of
them you should be able to have the intended experience out of the box. If you
are content editing the configuration files (which you will have to do
eventually) then all of the following should be easily replaceable.

If you notice any missing dependencies please raise an issue so that this table
can update.

| Dependency       | Description                                                                    | Why/Where is it needed?                                                                 |
| ---              | ---                                                                            | ---                                                                                     |
| `xmonad` v0.15+  | Window manager                                                                 | self-explanatory                                                                        |
| `xmonad-contrib` | Contributed additional functionality for xmonad                                | Everywhere, so much of the code base is these extras                                    |
| `dzen2`          | General purpose messaging and notification program                             | Power menu                                                                              |
| `dmenu2`         | General purpose menu                                                           | Application launcher, jump to window, bring window                                      |
| `gnome-session`  | Enables keyring and appearance                                                 | Loading all gnome session preferences (gtk appearance etc.)                             |
| `xset`           | User preference utility for X                                                  | Enabling/Disabling DPMS and screensaver                                                 |
| `feh`            | Image viewer and wallpaper setter                                              | Wallpaper                                                                               |
| `polybar`        | Status bar                                                                     | Workspace info, layout info, launchers, DPMS toggling, mpris2 info, time, volume, power |
| `compton`        | Compositor                                                                     | Shadows and glitz                                                                       |
| `i3lock-color`   | Screen locking                                                                 | self-explanatory                                                                        |
| `dunst`          | Notification daemon                                                            | self-explanatory                                                                        |
| `pulseaudio`     | Sound system                                                                   | Liberal use made of pacmd and pactl, anything audio related                             |
| M+               | Font used widely across the theme, get it [here](https://mplus-fonts.osdn.jp/) | self-explanatory                                                                        |
| Iosevka Custom   | Fonts (see [here](https://github.com/elenapan/dotfiles))                       | self-explanatory                                                                        |
| Deadhead Script  | Lock screen font ([here](https://www.dafont.com/deadhead-script.font))         | self-explanatory                                                                        |
| FontAwesome      | Icons                                                                          | self-explanatory                                                                        |
| `nix`            | A purely functional package manager                                            | Compilation of the xmonad binary                                                        |
| `fd`             | A replacement for `find`                                                       | It is in the `build` script to find the compiled xmonad version                         |

For the polybar configuration files please see
[here](https://github.com/karetsu/nix-overlays). It uses home-manager and
contains an overlay for nixpkgs with additional software. You also need my
custom scripts directory available [here](https://github.com/karetsu/scripts).

I have not mentioned here that the preferred login manager here is lightdm in
order to get access to `dm-tool` to enable user switching. If you do *not* use
lightdm then you will need to edit the polybar configuration and change the user
switching module into one which suits your preference.


## Default applications

This setup is pretty opinionated. I have some explicit applications named in
`./lib/App/Alias.hs` which you may wish to change to your own preferences.

| Function | Choice    |
| ---      | ---       |
| Terminal | `kitty`   |
| Browser  | `firefox` |
| Mail     | `geary`   |
| Music    | `plexamp` |
| IDE      | `emacs`   |

If you prefer other apps then edit the variables in this file. At some point you
will probably need to be doing this anyway and it helps you to get familiar with
the structure of my environment.


## Installation instructions

In order to compile this you need to have `nix-shell` and `zsh` in your $PATH
but once you have this it should be pretty automatic

``` sh
git clone https://gitlab.com/karetsu/xmonad-aloysius
cd xmonad-aloysius
./build -h
```

This will print the build help documentation. Follow the options presented to
build the binary to your needs.
