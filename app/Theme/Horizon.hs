-- |

module Theme.Horizon where

--basebg = "#232530"
basebg = "#1c1e26"
basefg = "#c7c9cb"
base00 = "#16161c"  -- darkest black
base01 = "#1a1c23"  -- dark black
base02 = "#1c1e26"  -- black
base03 = "#232530"  -- bright black
base04 = "#6a6a6a"  -- dark white
base05 = "#f9cec3"  -- white
base06 = "#f9cbbe"  -- bright white
base07 = "#fadad1"  -- teal
base08 = "#fdf0ed"  -- cyan
base09 = "#21bfc2"  -- blue
base10 = "#25b2bc"  -- dark blue
base11 = "#e95678"  -- red
base12 = "#f09383"  -- orange
base13 = "#fab795"  -- yellow
base14 = "#09f7a0"  -- green
base15 = "#b877db"  -- violet

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
