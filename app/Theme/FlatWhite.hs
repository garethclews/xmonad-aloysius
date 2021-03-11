-- |

module Theme.FlatWhite where

--basebg = "#232530"
basebg = "#f7f3ee"
basefg = "#605a52"
base00 = "#605a52" -- darkest black
base01 = "#93836c" -- dark black
base02 = "#b9a992" -- black
base03 = "#dcd3c6" -- bright black
base04 = "#e4ddd2" -- dark white
base05 = "#f1ece4" -- white
base06 = "#f7f3ee" -- bright white
base07 = "#5f8c7d" -- teal
base08 = "#81895d" -- cyan
base09 = "#4c5361" -- blue
base10 = "#7382a0" -- dark blue
base11 = "#5b4343" -- red
base12 = "#5b5143" -- orange
base13 = "#465953" -- yellow
base14 = "#84bd00" -- green
base15 = "#614c61" -- violet

fontsize :: String
fontsize = "12"

sansserif :: String
sansserif = "xft:Rounded Mplus 1c Medium:style=Regular:size=" ++ fontsize

sansserif' :: String
sansserif' = "Rounded Mplus 1c Medium-" ++ fontsize

monospace :: String
monospace = "xft:mplus Nerd Font Mono:size=" ++ fontsize

altsans :: String
altsans = "xft:mplus Nerd Font:style=Medium:size=" ++ fontsize
