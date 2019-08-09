-- | XMonad-Aloysius setup

module Config.Options where

import Data.Monoid

import XMonad

import XMonad.Actions.UpdatePointer

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.SpawnOnce

import qualified XMonad.Prompt as P

-- import Theme.Dracula
import App.Alias
import Config.Projects
import Theme.Nord


-- preferences ------------------------------------------------------------------
data Options = Options
  { term       :: String
  , ffm        :: Bool
  , mask       :: KeyMask
  , spaces     :: [String]
  , events     :: Event  -> X All
  , logs       :: X ()
  , starts     :: X ()
  }

options :: Options
options = Options
  { term       = tty
  , ffm        = True
  , mask       = mod4Mask
  , spaces     = wsList
  , events     = ewmhDesktopsEventHook
  , logs       = updatePointer (0.5, 0.5) (0, 0)
               >> spawn logger
  , starts     = ewmhDesktopsStartup
               >> setWMName "XMonad"
               -- apps from alias
               >> spawnOnce panel
               >> spawnOnce wallpaper
               >> spawnOnce compositor
               >> spawnOnce cursor
               >> spawnOnce lang
               >> spawnOnce notifications
               >> spawnOnce xresource
               >> spawnOnce energyStar
               -- return ()
  }


promptConfig :: P.XPConfig
promptConfig = P.def { P.fgColor = base04
                     , P.bgColor = basebg
                     , P.font = sansserif
                     , P.promptBorderWidth = 0
                     , P.height = 52
                     , P.defaultText = " "
                     , P.historySize = 0
                     , P.maxComplRows = Just 0
                     , P.position = P.Top
                     }
