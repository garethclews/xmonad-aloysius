-- | XMonad-Aloysius dracula colour scheme

module Dracula where

import XMonad

data Theme = Theme
  { foreground :: String
  , background :: String
  , highlight  :: String
  , focussed   :: String
  , unfocussed :: String
  , border     :: Dimension
  }

theme :: Theme
theme = Theme
  { highlight  = "#bbe9fd"
  , background = "#282a36"
  , foreground = "#dee3e0"
  , focussed   = "#ff79c6"
  , unfocussed = "#44475a"
  , border     = 1
  }
