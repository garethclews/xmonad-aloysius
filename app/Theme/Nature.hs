-- | A blue-green theme with a watermelon twist

module Theme.Nature where


basebg = "#232d39"
basefg = "#d639ff"
base00 = "#050709"  -- darkest black
base01 = "#0b0f12"  -- dark black
base02 = "#11161c"  -- black
base03 = "#171e25"  -- bright black
base04 = "#4a5158"  -- dark white
base05 = "#777c82"  -- white
base06 = "#a4a8ab"  -- bright white
base07 = "#d1d3df"  -- teal
base08 = "#ffffff"  -- cyan
base09 = "#1f8498"  -- blue
base10 = "#34ddfe"  -- dark blue
base11 = "#5ce3fe"  -- red
base12 = "#29cb94"  -- orange
base13 = "#5cfec7"  -- yellow
base14 = "#34feba"  -- green
base15 = "#fe5c93"  -- violet

fontsize :: String
fontsize = "12"

sansserif :: String
sansserif = "xft:Overpass:size=" ++ fontsize

sansserif' :: String
sansserif' = "Overpass-" ++ fontsize

monospace :: String
monospace = "xft:Iosevka Custom:size=" ++ fontsize
