{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- XMonad-Aloysius xmonad layouts

module Container.Layout where


-- Imports ----------------------------------------------------------------------
import           XMonad
import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Decoration
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.IfMax
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowSwitcherDecoration

import           XMonad.StackSet               as W

import           Config.Projects
import           Config.Options
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

-- Declarations
data Gaps' = Gaps'
  { u  :: Int
  , d  :: Int
  , x  :: Int
  , x' :: Integer
  }


newtype SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  describeDeco _ = "Side handles"
  shrink b (Rectangle _ _ dw dh) (Rectangle _x _y w h)
    | SideDecoration U <- b = Rectangle _x (_y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle _x _y (w - dw) h
    | SideDecoration D <- b = Rectangle _x _y w (h - dh)
    | SideDecoration L <- b = Rectangle (_x + fi dw) _y (w - dw) h
  pureDecoration b dw dh _ st _ (win, Rectangle _x _y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle _x _y w dh
      SideDecoration R -> Rectangle (_x + fi (w - dw)) _y dw h
      SideDecoration D -> Rectangle _x (_y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle _x _y dw h
    | otherwise = Nothing
  -- decorationCatchClicksHook _ _ _ _ = return True --
  -- This one needs to allow movement of the window as well...

-- Settings ---------------------------------------------------------------------

gs :: Gaps'
gs = Gaps' { u = 20, d = 20, x = 20, x' = 20 }


gapses :: l a -> ModifiedLayout Gaps l a
gapses = gaps [(U, u gs), (R, x gs), (L, x gs), (D, d gs)]

spacingses :: l a -> ModifiedLayout Spacing l a
spacingses = spacingRaw True
                        (Border 0 (x' gs) (x' gs) (x' gs))
                        True
                        (Border (x' gs) (x' gs) (x' gs) (x' gs))
                        True

-- my decoration Aloy DECOrations
sidedeco
  :: Eq a
  => l a
  -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
sidedeco = decoration shrinkText sideDecoTheme (SideDecoration L)


-- customised layouts
full = named "Fullscreen" $ noBorders (fullscreenFull Full)

bsp =
  named "Binary Partition"
    . IfMax 1 full
    . gapses
    . windowSwitcherDecoration shrinkText decoTheme
    . draggingVisualizer
    . spacingses
    $ emptyBSP

tall =
  named "Tall"
    . IfMax 1 full
    . gapses
    . windowSwitcherDecoration shrinkText decoTheme
    . draggingVisualizer
    . spacingses
    $ ResizableTall 1 (2 / 100) (1 / 2) []

tcm =
  named "Three Columns"
    . IfMax 1 full
    . gapses
    . draggingVisualizer
    . windowSwitcherDecoration shrinkText decoTheme
    . spacingses
    $ ThreeColMid 1 (1 / 10) (1 / 2)

tabs = named "Tabbed" $ tabbedBottom shrinkText tabTheme

flt = named "Float" . sidedeco $ simpleFloat' shrinkText emptyTheme


-- layout --
layout =
  avoidStruts
    .   smartBorders
    .   onWorkspace wsScratch flt
    $   tcm
    ||| tall
    ||| bsp
    ||| tabs
