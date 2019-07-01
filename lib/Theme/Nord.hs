-- | XMonad-Aloysius nord colour scheme
-- https://www.nordtheme.com

module Theme.Nord where

import XMonad

data Theme = Theme
  { foreground :: String
  , background :: String
  , highlight  :: String
  , focused    :: String
  , unfocused  :: String
  , border     :: Dimension
  }

-- whole nord colour palette
nord00 = "#2E3440"
nord01 = "#3B4252"
nord02 = "#434C5E"
nord03 = "#4C566A"
nord04 = "#D8DEE9"
nord05 = "#E5E9F0"
nord06 = "#ECEFF4"
nord07 = "#8FBCBB"
nord08 = "#88C0D0"
nord09 = "#81A1C1"
nord10 = "#5E81AC"
nord11 = "#BF616A"
nord12 = "#D08770"
nord13 = "#EBCB8B"
nord14 = "#A3BE8C"
nord15 = "#B48EAD"

sansserif :: String
sansserif = "xft:Fira Sans:size=10"

monospace :: String
monospace = "xft:Fira Mono:size=10"


theme :: Theme
theme = Theme
  { highlight  = nord03
  , background = nord00
  , foreground = nord04
  , focused    = nord02
  , unfocused  = nord01
  , border     = 1
  }
