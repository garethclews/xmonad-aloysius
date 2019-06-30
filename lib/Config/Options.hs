-- | XMonad-Aloysius setup

module Config.Options where

import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid
import System.Exit
import System.IO (hClose, hFlush, Handle)

import XMonad

import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Actions.ShowText
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.AvoidFloats
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.Dishes
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- import Theme.Dracula
import App.Alias
import Theme.Nord

-- personal preferences for use
data Options = Options
  { term   :: String
  , ffm    :: Bool
  , mask   :: KeyMask
  , spaces :: [String]
  , events :: Event  -> X All
  , logs   :: X ()
  , starts :: X ()
  }

options = Options
  { term   = "urxvt"
  , ffm    = True
  , mask   = mod4Mask
  , spaces = map show [1..4]
  , events = ewmhDesktopsEventHook
  , logs   = updatePointer (0.5, 0.5) (0, 0)
           >> spawn logger
  , starts = ewmhDesktopsStartup
             >> setWMName "XMonad"
             -- apps from alias
             >> spawnOnce panel
             >> spawnOnce wallpaper
             >> spawnOnce compositor
             >> spawnOnce cursor
             >> spawnOnce lang
             >> spawnOnce notifications
             >> spawnOnce xresource
             -- return ()
  }

