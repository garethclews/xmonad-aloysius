{-
____             _    _                 _
\ \ \ _____     / \  | | ___  _   _ ___(_)_   _ ___
 \ \ \_____|   / _ \ | |/ _ \| | | / __| | | | / __|
 / / /_____|  / ___ \| | (_) | |_| \__ \ | |_| \__ \
/_/_/        /_/   \_\_|\___/ \__, |___/_|\__,_|___/
                              |___/

---------------------------------------------------------------------------
-- https://gitlab.com/karetsu                                            --
-- X. >= 0.15
---------------------------------------------------------------------------
-}

---------------------------------------------------------------------------
-- TODO
---------------------------------------------------------------------------
{-|
  GENERAL

  * polybar pimping:
    - better workspace listing
      + should I replace with dzen to get a clickable ws list?
  * keybindings are currently all over the place
    - get toggle float all and sink all
    - helper scripts to throw in ~/.scripts
    - see how wmctl can help me out
  * investigate how to add i3 niceties (maximise on only window)
  * X.U.Dzen to replicate the kind of bar on the left of:
    https://i.redd.it/glzrkk83f4621.png ???
    -- could this be a dzen-based scratchpad?
    -- consider adding X.H.ScreenCorners to add in mouse activation on left edge
  * Incorporate goodness from:
    https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs
  * current dmenu is a bit clumsy on spacing, make it avoidStruts rather than place
    itself at 40 pixels so make it work on multihead setups
  * Design a sound scheme


  NON XMONAD SPECIFIC
  * look into using glances to generate JSON for outputting somehow


  ACTIVE


  DEFER
  * https://github.com/pjones/xmonadrc has some dynamic project helper functions


  DONE
  * tidy up gaps with polybar
    - pushed gaps over |||, now full screen is as intended
  * pimped polybar with current layout descriptor (pprint)
  * make fullscreen logout
  * swapped out rofi for X.U.Dmenu
  * implemented dynamic projects
  * M-M1 now uses flexible manipulate (discrete - window is 9 blocks)
  * added scratchpads
  * sort out xautolock to prevent locking on screen with video playing
    - need to get a toggleable `dset +-dpms` keybind with writes to fifo for polybar integration
    - xset -dpms seems to not behave as intended, do we need xscreensaver installed?
  * xset oddity where any window activity sees the polybar icon turn back on, this was
    down to it being part of the log-hook so activated incorrectly on any action


  TESTED/REJECTED/WONTFIX
  * X.U.Themes - see if nord and dracula can be added -- going to just keep my
    themes in my format, if anybody else ever wants to use it then maybe revisit
    but just for me there's little to no point
  * XMonad.Hooks.DynamicBars  -- sticking with one bar

 -}

-- Imports ----------------------------------------------------------------------
import Control.Monad

import XMonad

import XMonad.Actions.DynamicProjects

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Replace
import XMonad.Util.Run

-- Personal imports (./lib/)
import App.Scratchpad
import Bind.Master
import Bus.Events
import Config.Options
import Config.Projects
import Container.Layout
import Container.Navigation
import Theme.Nord -- alternatively Dracula


-- Configuration ----------------------------------------------------------------
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults = def {
  -- simple stuff
  terminal           = term options,
  focusFollowsMouse  = ffm options,
  modMask            = mask options,
  workspaces         = spaces options,

  normalBorderColor  = unfocused theme,
  focusedBorderColor = focused theme,
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
hooks :: ManageHook
hooks = composeOne
  [ className =? "Places" -?> doFloat
  , isDialog             -?> doCenterFloat
  , transience -- I don't actually understand what this does
  , pure True            -?> insertPosition End Newer
  ]
  <+> manageDocks
  <+> manageScratchpad


-- Main -------------------------------------------------------------------------
main :: IO ()
main = do
    replace

    -- pipes
    forM_ [ "xmonad-ws"
          , "xmonad-layout"
          , "caffeine"
          ]
          $ \file -> safeSpawn "mkfifo" ["/tmp/"++file]


    -- set up our ewmh-based desktop
    xmonad
      . docks
      . ewmh
      . navigate
      . dynamicProjects projects
      $ defaults
