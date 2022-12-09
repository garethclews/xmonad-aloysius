-- | Personal 'programs as functions' list
-- This is here so that I can change the programs behind things without worry
-- about propogating all the changes. All functions external to haskell should
-- be specified here.

module App.Alias where


settingsd :: String
settingsd = "xsettingsd &"

panel :: String
-- panel = "~/.scripts/polybar/launch"
panel = "polybar &"


wallpaper :: String
wallpaper = "~/.fehbg"

cursor :: String
cursor = "xsetroot -cursor_name left_ptr &"

lang :: String
lang = "export LANG=en_GB.UTF-8"

screensaver :: String
screensaver = "~/.scripts/lock"

suspend :: String
suspend = "~/.scripts/suspend"

energyStar :: String
energyStar = "echo '%{F#eceff4}\xf111%{F-}\n' > /tmp/xmonad-caffeine"

xresource :: String
xresource =
  "[[ -f ~/.Xresources ]] && nohup xrdb -merge -I$HOME ~/.Xresources >/dev/null"

tty :: String
tty = "kitty -1"

scratch :: String
scratch = "kitty --name=scratchpad"

mail :: String
mail = "geary"

music :: String
-- music = "appimage-run ~/.scripts/plexamp"
music = "spotify"

code :: String
code = "emacs"

everywhere :: String
everywhere = "emacsclient --eval \"(emacs-everywhere)\""


browser :: String
browser = "firefox"

touchEvents :: String
touchEvents = "echo \"\xf111\" > /tmp/xmonad-events"

chat :: String
chat = "signal-desktop & discord"
