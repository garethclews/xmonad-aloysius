-- | XMonad-Aloysius xmonad layouts

module Layout where


-- Imports ----------------------------------------------------------------------
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run        (spawnPipe)
import XMonad.Util.EZConfig   (additionalKeys)
import XMonad.Util.SpawnOnce

import Data.List
import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO

import Data.Monoid
import System.Exit
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CopyWindow
import XMonad.Layout.WindowNavigation
import XMonad.Actions.WindowNavigation
import XMonad.Layout.Combo
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Layout.NoBorders
import XMonad.Actions.FloatKeys
import XMonad.Util.Replace
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.Master
import XMonad.Layout.Dishes
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Column
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Actions.ShowText
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.AvoidFloats
import System.IO (hClose, hFlush, Handle)
import Data.Maybe (fromMaybe, fromJust)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


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
data Gaps = Gaps
  { u :: Int
  , d :: Int
  , x :: Int
  }

gs = Gaps
  { u = 10
  , d = 10
  , x = 10
  }

layout = gaps [(U, u gs), (R, x gs), (L, x gs), (D, d gs)]
  $   smartBorders
  $   Column 1
  ||| avoidFloats (tiled)
  ||| Mirror tiled
  ||| noBorders (fullscreenFull Full)
  where
    tiled = Tall 1 (1/2) (3/10)
