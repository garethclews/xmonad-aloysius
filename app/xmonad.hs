{-
____             _    _                 _
\ \ \ _____     / \  | | ___  _   _ ___(_)_   _ ___
 \ \ \_____|   / _ \ | |/ _ \| | | / __| | | | / __|
 / / /_____|  / ___ \| | (_) | |_| \__ \ | |_| \__ \
/_/_/        /_/   \_\_|\___/ \__, |___/_|\__,_|___/
                              |___/

---------------------------------------------------------------------------------
-- https://gitlab.com/karetsu/xmonad-aloysius                                  --
-- X. >= 0.15                                                                  --
--                                                                             --
-- For pending changes see the todo                                            --
---------------------------------------------------------------------------------
-}

-- Imports ----------------------------------------------------------------------
import           XMonad

import           XMonad.Actions.DynamicProjects

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook

import           XMonad.Util.Replace
import           XMonad.Util.Run


import           XMonad.Util.XUtils


-- Personal imports (./lib/)
import           Bind.Master
import           Bus.EventHook
import           Bus.LogHook
import           Bus.ManageHook
import           Config.Options
import           Config.Projects
import           Container.Layout
import           Container.Navigation


-- Configuration ----------------------------------------------------------------
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults = def {
  -- simple stuff
                 terminal           = term options
               , focusFollowsMouse  = ffm options
               , modMask            = mask options
               , workspaces         = spaces options
               , normalBorderColor  = unfocused theme
               , focusedBorderColor = focused theme
               , borderWidth        = border theme

  -- key bindings
               , keys               = defaultKeys
               , mouseBindings      = mouseBindings'

  -- hooks, layouts
               , layoutHook         = layout
               , manageHook         = manager
               , handleEventHook    = eventer
               , logHook            = logHooker
               , startupHook        = starts options
               }


-- Sandpit ----------------------------------------------------------------------
newWin :: Rectangle -> X Window
newWin r = do
  let mask = Just $ exposureMask .|. buttonPressMask
  w <- createNewWindow r mask "#000000" False
  showWindow w
  d <- asks display
  liftIO $ lowerWindow d w
  return w



-- Main -------------------------------------------------------------------------
main :: IO ()
main = do
  replace

  -- pipes
  safeSpawn
    "mkfifo"
    [ "/tmp/xmonad-wspace"
    , "/tmp/xmonad-layout"
    , "/tmp/xmonad-events"
    , "/tmp/xmonad-states"
    , "/tmp/xmonad-copy"
    , "/tmp/xmonad-full"
    , "/tmp/xmonad-kill"
    , "/tmp/xmonad-caffeine"
    ]

  -- compose all the functionality
  xmonad
    . docks
    . ewmh
    . navigate
    . dynamicProjects projects
    . withUrgencyHook NoUrgencyHook
    $ defaults

