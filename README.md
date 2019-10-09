# [Call Me Al](https://www.youtube.com/watch?v=uq-gYOrU8bA)

A multi-themed XMonad configuration by Aloysius.


## Details

This XMonad configuration is my attempt at making it feel more like a desktop
environment featuring a lot more mod-cons than you typically find in a tiling
WM. You will also need my polybar configuration, available in the wiki, as well
as all of the supporting scripts available
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
issue. As the build is managed in a nix-shell then it should "just work".


## Latest Previews

| Nord everywhere                                | Named scratchpad popup                         | Lock screen                                                                     |
| ---                                            | ---                                            | ------------------------------------------------------------------------------- |
| ![Screenshot](https://i.imgur.com/eRHOmQT.png) | ![Screenshot](https://i.imgur.com/OC36W58.png) | ![Screenshot](https://i.imgur.com/MVvi8RU.png)                                  |


## Features

This list contains all of the features not included in the default XMonad
experience. Key sequences are modal with <kbd>M</kbd> representing your left
super, <kbd>S-</kbd> means shift plus the following button and <kbd>C-</kbd> is control
plus whatever follows it

| Feature                             | Description                                                                                                         | Access                                                                   |
| ---                                 | ---                                                                                                                 | ---                                                                      |
| EWMH                                | Simpler control over X                                                                                              | always on                                                                |
| Dynamic Projects                    | Each workspace is given as a 'project' with default applications launched on entry if empty                         | always on                                                                |
| Easy theming                        | Nominate your 16 base colours and preferred fonts and they appear everywhere                                        | always on                                                                |
| Modal keybindings                   | Vim style keybind sequences                                                                                         | always on                                                                |
| Tabbed layout                       | For when you want lots of containers on one workspace and clickable tabs                                            | always on                                                                |
| Custom event hooks                  | FIFO for easy piping of event text to outside applications                                                          | always on                                                                |
| Media keys                          | Using media keys via playerctl                                                                                      | always on                                                                |
| Maximise on single window           | Inspired by the i3-gaps treatment where gaps are disabled for single windows, this applies to all non-float layouts | always on                                                                |
| Named Scratchpad                    | Pop over terminal similar to quake/yakuake                                                                          | <kbd>M</kbd> <kbd>\`</kbd>                                               |
| Go-to window                        | X.A.WindowBringer - go to windows dmenu prompt for quick jumping between workspaces                                 | <kbd>M</kbd> <kbd>w</kbd> <kbd>g</kbd> or click on workspaces on polybar |
| Bring window                        | As above, but this time brings a window to your current workspace                                                   | <kbd>M</kbd> <kbd>w</kbd> <kbd>b</kbd>                                   |
| Search selected                     | Launch a web search for the currently selected text*                                                                | <kbd>M</kbd> <kbd>/</kbd> <kbd>s</kbd> \<search option\>                 |
| Search prompt                       | Launch a dmenu prompt to launch a web search*                                                                       | <kbd>M</kbd> <kbd>/</kbd> <kbd>p</kbd> \<search option\>                 |
| Toggling DPMS                       | Polybar clickable icon to toggle `xset +-dpms` and `xset s on/off`                                                  | click on lightbulb                                                       |
| Lock screen                         | Provided by i3lock-color & used in lock and suspend                                                                 | <kbd>M</kbd> <kbd>q</kbd> <kbd>l</kbd> or lock in polybar                |
| Dedicated floating window workspace | A workspace where all windows float (workspace 7 by default)                                                        | <kbd>M</kbd> <kbd>7</kbd>                                                |
| Navigation2D                        | Swap windows on workspaces, go to windows and go to screens with keyboard shortcuts                                 | <kbd>M</kbd> { ,<kbd>S-</kbd>,<kbd>C-</kbd>} \<arrow keys\>              |
| XMonad prompt                       | Prompt to run lesser used xmonad actions                                                                            | <kbd>M</kbd> <kbd>/</kbd> <kbd>/</kbd>                                   |

* Search options
  - g: duckduckgo
  - h: hoogle
  - w: wikipedia


## Dependencies

Here are all of the things which this setup needs to work. If you install all of
them you should be able to have the intended experience out of the box. If you
are content editing the configuration files (which you will have to do
eventually) then all of the following should be easily replaceable.

If you notice any missing dependencies please raise an issue so that this table
can be updated.

| Dependency       | Description                                                            | Why/Where is it needed?                                                                  |
| ---              | ---                                                                    | ---                                                                                      |
| `xmonad` v0.15+  | Window manager                                                         | self-explanatory                                                                         |
| `xmonad-contrib` | Contributed additional functionality for xmonad                        | Everywhere, so much of the code base is these extras                                     |
| `dzen2`          | General purpose messaging and notification program                     | Power menu                                                                               |
| `dmenu2`         | General purpose menu                                                   | Application launcher, jump to window, bring window                                       |
| `xset`           | User preference utility for X                                          | Enabling/Disabling DPMS and screensaver                                                  |
| `feh`            | Image viewer and wallpaper setter                                      | Wallpaper                                                                                |
| `polybar`        | Status bar                                                             | Workspace info, layout info, launchers, DPMS toggling, spotify info, time, volume, power |
| `compton`        | Compositor                                                             | Shadows and glitz                                                                        |
| `i3lock-color`   | Screen locking                                                         | self-explanatory                                                                         |
| `dunst`          | Notification daemon                                                    | self-explanatory                                                                         |
| `pulseaudio`     | Sound system                                                           | Liberal use made of pacmd and pactl, anything audio related                              |
| Iosevka Custom   | Fonts (see [here](https://github.com/elenapan/dotfiles))               | self-explanatory                                                                         |
| Deadhead Script  | Lock screen font - [here](https://www.dafont.com/deadhead-script.font) | self-explanatory                                                                         |
| FontAwesome      | Icons                                                                  | self-explanatory                                                                         |
| `nix`            | A purely functional package manager                                    | Compilation of the xmonad binary                                                         |
| `fd`             | A replacement for `find`                                               | It is used in the `build` script to find the compiled xmonad version                     |

For the polybar configuration files please see
[here](https://github.com/karetsu/nix-overlays). This is managed by home-manager and
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
| Music    | `spotify` |
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

This will print the build help documentation. Follow the options presented here
to build the binary to your needs.


## Key Bindings

Installing is all well and good but if you log in and don't know how to launch
anything there could be trouble. Familiarise yourself with some of these useful
bindings. A full list will be available on the wiki.

In all of the below, <kbd>M</kbd> is the Windows key (or whatever your Mod4 has
been changed to) and <kbd>S</kbd> is Shift.

| Key Binding                              | Action                             |
|------------------------------------------|------------------------------------|
| <kbd>M</kbd> <kbd>Return</kbd>           | Launch terminal                    |
| <kbd>M</kbd> <kbd>p</kbd>                | Spawn dmenu to launch applications |
| <kbd>M</kbd> <kbd>Space</kbd>            | Change to the next layout          |
| <kbd>M</kbd> <kbd>Tab</kbd>              | Move to next window                |
| <kbd>M</kbd> <kbd>S</kbd>-<kbd>Tab</kbd> | Move to previous window            |
| <kbd>M</kbd> <kbd>q</kbd> <kbd>q</kbd>   | Quit XMonad                        |
| <kbd>M</kbd> <kbd>q</kbd> <kbd>m</kbd>   | Open the power menu                |
| <kbd>M</kbd> <kbd>1</kbd>..<kbd>7</kbd>  | Change to workspace 1 thro' 7      |
| <kbd>M</kbd> <kbd>\`</kbd>               | Launch scratchpad terminal         |
| <kbd>M</kbd> <kbd>a</kbd> <kbd>k</kbd>   | Close application                  |
