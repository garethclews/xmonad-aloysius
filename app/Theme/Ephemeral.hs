-- | based on EmpressNoodle's ephemeral theme

module Theme.Ephemeral where

basebg = "#323f4e"
basefg = "#f8f8f2"
base00 = "#181e26"  -- darkest black
base01 = "#1e262d"  -- dark black
base02 = "#56687e"  -- black
base03 = "#2a3542"  -- bright black
base04 = "#242d39"  -- dark white
base05 = "#f8f8f2"  -- white
base06 = "#fdfdfd"  -- bright white
base07 = "#a1efd3"  -- teal
base08 = "#87dfeb"  -- cyan
base09 = "#92b6f4"  -- blue
base10 = "#6498ef"  -- dark blue
base11 = "#f48fb1"  -- red
base12 = "#53e2ae"  -- orange
base13 = "#f1fa8c"  -- yellow
base14 = "#53e2ae"  -- green
base15 = "#bd99ff"  -- violet

fontsize :: String
fontsize = "12"

sansserif :: String
sansserif = "xft:Fira Sans:size=" ++ fontsize

sansserif' :: String
sansserif' = "Fira Sans-" ++ fontsize

monospace :: String
monospace = "xft:Iosevka Nerd Font:size=" ++ fontsize
