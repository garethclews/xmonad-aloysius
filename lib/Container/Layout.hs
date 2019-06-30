-- | XMonad-Aloysius xmonad layouts

module Container.Layout where


-- Imports ----------------------------------------------------------------------
import XMonad
import XMonad.Hooks.ManageDocks

import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders


------------------------------------------------------------------------
--
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
data Gaps' = Gaps'
  { u :: Int
  , d :: Int
  , x :: Int
  }

gs :: Gaps'
gs = Gaps'
  { u = 44
  , d = 20
  , x = 20
  }

gapses = gaps [(U, u gs), (R, x gs), (L, x gs), (D, d gs)]

-- layout --
layout = avoidStruts
       $ gapses
       $ smartBorders
       $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
       $ resize
     ||| emptyBSP
     ||| full
  where
    resize = ResizableTall 1 (2/100) (1/2) []
    full = noBorders (fullscreenFull Full)
