-- | XMonad-Aloysius setup

module Config.Options where

import Data.Monoid

import XMonad

import XMonad.Actions.UpdatePointer

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.SpawnOnce

-- import Theme.Dracula
import App.Alias
import Config.Projects


-- preferences ------------------------------------------------------------------
data Options = Options
  { term   :: String
  , ffm    :: Bool
  , mask   :: KeyMask
  , spaces :: [String]
  , events :: Event  -> X All
  , logs   :: X ()
  , starts :: X ()
  }

options :: Options
options = Options
  { term   = tty
  , ffm    = True
  , mask   = mod4Mask
  , spaces = wsList
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
