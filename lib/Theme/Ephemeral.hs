-- | based on EmpressNoodle's ephemeral theme

module Theme.Ephemeral where

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
base00 = "#3d4c5f"
base01 = "#f48fb1"
base02 = "#a1efd3"
base03 = "#F1FA8C"
base04 = "#92B6F4"
base05 = "#BD99FF"
base06 = "#87DFEB"
base07 = "#F8F8F2"
base08 = "#56687E"
base09 = "#EE4F84"
base10 = "#53E2AE"
base11 = "#F1FF52"
base12 = "#6498EF"
base13 = "#985EFF"
base14 = "#24D1E7"
base15 = "#E5E5E5"
-- additional
basebg = "#323F4E"
basefg = "#f8f8f2"
basecc = "#f8f8f2"


sansserif :: String
sansserif = "xft:Fira Sans:size=10"

monospace :: String
monospace = "xft:Fira Mono:size=10"


theme :: Theme
theme = Theme
  { highlight  = base03
  , background = basebg
  , foreground = basefg
  , focused    = base02
  , unfocused  = base01
  , border     = 1
  }
