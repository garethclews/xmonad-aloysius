-- | Personal 'programs as functions' list

module Aliases where

dmenu :: String
dmenu = "rofi -show combi"

panel :: String
panel = "dzen2 -ta l -p -w 400 -y 10 -x 10 -h 24 -e ''"

conky :: String
conky = "conky -c ~/.conky/top.conky | dzen2 -x 400 -y 10 -h 24 -w 2150 -p -ta r -e ''"
