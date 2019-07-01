-- | XMonad-Aloysius dracula colour scheme

module Theme.Dracula where

import XMonad

data Theme = Theme
  { foreground :: String
  , background :: String
  , highlight  :: String
  , focused    :: String
  , unfocused  :: String
  , border     :: Dimension
  }

sansserif :: String
sansserif = "xft:Fira Sans:size=10"

monospace :: String
monospace = "xft:Fira Mono:size=10"


theme :: Theme
theme = Theme
  { highlight  = "#bbe9fd"
  , background = "#282a36"
  , foreground = "#dee3e0"
  , focused    = "#ff79c6"
  , unfocused  = "#44475a"
  , border     = 1
  }
