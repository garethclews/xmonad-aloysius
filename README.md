# Call Me Aloysius

A [Nord](https://www.nordtheme.com/) themed XMonad configuration.

## Details

TODO

## Latest Preview

![](./screens/1.png)
![](./screens/2.png)


## Dependencies

Here are all of the things which this setup needs to work. If you install all of
them you should be able to have the intended experience out of the box. If you
are content editing the configuration files (which you will have to do
eventually) then all of the following should be easily replaceable.

If you notice any missing dependencies please raise an issue so that this table
can be updated.

| Dependency      | Description                                        | Why/Where is it needed?                                                                  |
| ---             | ---                                                | ---                                                                                      |
| `xmonad` v0.15+ | Window manager                                     | self-explanatory                                                                         |
| `dzen2`         | General purpose messaging and notification program | Power menu                                                                               |
| `dmenu2`        | General purpose menu                               | Application launcher, jump to window, bring window                                       |
| `feh`           | Image viewer and wallpaper setter                  | Wallpaper                                                                                |
| `polybar`       | Status bar                                         | Workspace info, layout info, launchers, DPMS toggling, spotify info, time, volume, power |
| `i3lock-fancy`  | Screen locking                                     | self-explanatory                                                                         |
| `dunst`         | Notification daemon                                | self-explanatory                                                                         |
| Fira Sans+Mono  | Fonts                                              | self-explanatory                                                                         |
| FontAwesome     | Icons                                              | self-explanatory                                                                         |

For the polybar configuration files please see
[here](https://github.com/nix-overlays). This is managed by home-manager and
contains an overlay for nixpkgs with additional software.


## Default applications

This setup is pretty opinionated. I have some explicit applications named in
`./lib/App/Alias.hs` which you may wish to change to your own preferences.

| Function | Choice    |
| ---      | ---       |
| Terminal | `urxvt`   |
| Browser  | `firefox` |
| Mail     | `geary`   |
| Music    | `spotify` |
| IDE      | `emacs`   |

If you prefer other apps then edit them in this file.


# TODO: finish this doc
