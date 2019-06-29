{-
  yay, xmonad

  inspired by:
    https://blog.pclewis.com/2016/03/19/xmonad-spacemacs-style.html
    https://git.sr.ht/~ben/cfg/tree/master/xmonad.hs
-}

import Control.Monad (forM_, join)

import Data.Monoid
import Data.Maybe (fromMaybe, fromJust)
import System.Exit

import XMonad

import XMonad.Actions.ShowText
import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


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
        keys               = mainKeymap,
        mouseBindings      = mouseBindings',

      -- hooks, layouts
        layoutHook         = layout,
        manageHook         = hooks,
        handleEventHook    = events options,
        --logHook            = logs options,
        logHook            = eventLogHook,
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
  [ className =? "pinentry-gtk-2" -?> doFloat
  , isDialog                      -?> doCenterFloat
  , transience -- I don't actually understand what this does
  , isFullscreen                  -?> doFullFloat
  , pure True                     -?> insertPosition End Newer
  ] <+> manageDocks
  where role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    replace

    forM_ ["xmonad-ws"] $ \file -> do  -- TODO: expand later
      safeSpawn "mkfifo" ["/tmp/"++file]

    config <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) defaults
    xmonad
      . docks
      . ewmh
      $ config `additionalKeys` ([((m .|. mask options, k), windows $ f i)
                                 | (i, k) <- zip (spaces options) [xK_1 .. xK_9]
                                 , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])

