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
-- naming convention for my personal themes, all should have 16 base colours
basebg = "#292E39"
base00 = "#2E3440"
base01 = "#3B4252"
base02 = "#434C5E"
base03 = "#4C566A"
base04 = "#D8DEE9"
base05 = "#E5E9F0"
base06 = "#ECEFF4"
base07 = "#8FBCBB"
base08 = "#88C0D0"
base09 = "#81A1C1"
base10 = "#5E81AC"
base11 = "#BF616A"
base12 = "#D08770"
base13 = "#EBCB8B"
base14 = "#A3BE8C"
base15 = "#B48EAD"


sansserif :: String
sansserif = "xft:Fira Sans:size=12"

sansserif' :: String
sansserif' = "Fira Sans-12"

monospace :: String
monospace = "xft:Fira Mono:size=12"


theme :: Theme
theme = Theme
  { highlight  = base03
  , background = base00
  , foreground = base04
  , focused    = base02
  , unfocused  = base01
  , border     = 1
  }
