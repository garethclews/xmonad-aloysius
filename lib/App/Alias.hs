-- | Personal 'programs as functions' list
-- This is here so that I can change the programs behind things without worry
-- about propogating all the changes. All functions external to haskell should
-- be specified here.

module App.Alias where

appLauncher :: String
-- appLauncher = "rofi -show run"
appLauncher = " dmenu_run -p 'Launch application:  ' \
              \ -fn 'Fira Sans-12' \
              \ -nb '#2e3440' \
              \ -nf '#d8dee9' \
              \ -sb '#a3be8c' \
              \ -sf '#2e3440' \
              \ -h 52 \
              \ -o 80 \
              \ -y 52"  -- playing with offsets to see if it skips the dock


panel :: String
panel = "~/.scripts/polybar/launch"


wallpaper :: String
wallpaper = "~/.fehbg"


compositor :: String
compositor = "systemctl --user start compton"


cursor :: String
cursor = "xsetroot -cursor_name left_ptr"


lang :: String
lang = "export LANG=en_GB.UTF-8"


numlock :: String
numlock = "exec numlockx"


notifications :: String
notifications = "dunst"


screensaver :: String
screensaver = "i3lock-fancy -t 'Hi Aloysius'"


energyStar :: String
energyStar = "echo '%{F#eceff4}\xf0eb%{F-}\n' > /tmp/caffeine "


xresource :: String
xresource = "[[ -f ~/.Xresources ]] && nohup xrdb -merge -I$HOME ~/.Xresources >/dev/null"


logger :: String
logger = "xdotool search dunst windowraise"


tty :: String
tty = "urxvt"


mail :: String
mail = "geary"


music :: String
music = "spotify"


code :: String
code = "emacs"


browser :: String
browser = "firefox"
