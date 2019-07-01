-- | Personal 'programs as functions' list
-- This is here so that I can change the programs behind things without worry
-- about propogating all the changes. All functions external to haskell should
-- be specified here.

module App.Alias where

dmenu :: String
dmenu = "rofi -show run"

panel :: String
panel = "~/.scripts/polybar/launch"

wallpaper :: String
wallpaper = "~/.fehbg"

compositor :: String
compositor = "compton --config ~/.config/compton/compton.conf"

cursor :: String
cursor = "xsetroot -cursor_name left_ptr"

lang :: String
lang = "export LANG=en_GB.UTF-8"

numlock :: String
numlock = "numlockx &"

notifications :: String
notifications = "dunst"

xresource :: String
xresource = "[[ -f ~/.Xresources ]] && nohup xrdb -merge -I$HOME ~/.Xresources >/dev/null"

logger :: String
logger = "xdotool search dunst windowraise"
