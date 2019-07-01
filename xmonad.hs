{-
____             _    _                 _
\ \ \ _____     / \  | | ___  _   _ ___(_)_   _ ___
 \ \ \_____|   / _ \ | |/ _ \| | | / __| | | | / __|
 / / /_____|  / ___ \| | (_) | |_| \__ \ | |_| \__ \
/_/_/        /_/   \_\_|\___/ \__, |___/_|\__,_|___/
                              |___/

---------------------------------------------------------------------------
-- https://github.com/karetsu                                            --
-- X. >= 0.15
---------------------------------------------------------------------------
-}

---------------------------------------------------------------------------
-- TODO
---------------------------------------------------------------------------
{-|
  GENERAL

  * look into X.A.DynamicProjects
  * XMonad.Hooks.DynamicBars
  * make fullscreen logout and a menu which allows keypresses bound for
  * polybar pimping:
    - include current layout
    - better workspace listing
  * keybindings are currently all over the place
    - get toggle float all and sink all
    - helper scripts to throw in ~/.scripts
    - see how wmctl can help me out
  * can we swap out rofi for xmonad-extras/contrib functions?
  * tidy up gaps with polybar
  * add scratchpads
  * investigate how to add i3 niceties (maximise on only window)
  * X.U.Dzen to replicate the kind of bar on the left of:
    https://i.redd.it/glzrkk83f4621.png
  * X.U.Themes - see if nord and dracula can be added
  * Incorporate goodness from:
    https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs


  NON XMONAD SPECIFIC
  * sort out xautolock to prevent locking on screen with video playing
    - see if caffeine is still the go to?


  ACTIVE


  DEFER
  * https://github.com/pjones/xmonadrc has some dynamic project helper functions
  * investigate X.A.Navigation2D


  DONE


  TESTED/REJECTED/WONTFIX

 -}


import Control.Monad (forM_, join)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid
import System.Exit
import XMonad

import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Actions.ShowText
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run

import qualified Data.Map        as M
import qualified XMonad.StackSet as W


-- Personal imports (./lib/)
import App.Alias
import Bind.Master
import Bus.Events
import Config.Options
import Container.Layout
import Theme.Nord -- alternatively Dracula


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults = def {
  -- simple stuff
  terminal           = term options,
  focusFollowsMouse  = ffm options,
  modMask            = mask options,
  workspaces         = spaces options,

  normalBorderColor  = unfocussed theme,
  focusedBorderColor = focussed theme,
  borderWidth        = border theme,

  -- key bindings
  keys               = defaultKeys,
  mouseBindings      = mouseBindings',

  -- hooks, layouts
  layoutHook         = layout,
  manageHook         = hooks,
  handleEventHook    = events options,
  logHook            = logHook',
  startupHook        = starts options
}


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
hooks = composeOne
  [ isDialog   -?> doCenterFloat
  , transience -- I don't actually understand what this does
  , pure True  -?> insertPosition End Newer
  ] <+> manageDocks


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = do
    replace

    -- polybar pipes
    forM_ [ "xmonad-ws"
          , "xmonad-mode"
          , "xmonad-layout"
          ]
          $ \file -> do  -- TODO: expand later
             safeSpawn "mkfifo" ["/tmp/"++file]

    -- set up our ewmh
    xmonad
      . docks
      . ewmh
      $ defaults
