-- |

module Bind.Master where

import System.Exit

import XMonad

import XMonad.Actions.WindowBringer

import XMonad.Layout.AvoidFloats

-- build custom prompt to help find keys
-- https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Prompt.html

import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Ungrab

import qualified Data.Map                          as M
import qualified XMonad.Actions.FlexibleManipulate as F
import qualified XMonad.Actions.Search             as S
import qualified XMonad.Prompt                     as P
import qualified XMonad.StackSet                   as W


-- local
import App.Alias
import App.Launcher
import Config.Options

-- Keymaps ----------------------------------------------------------------------

-- default keymap
defaultKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
defaultKeys c = mkKeymap c $
  [ ("M-<Return>"  , spawn (term options))
  , ("M-p"         , spawn appLauncher)
  , ("M-<Space>"   , sendMessage NextLayout)
  , ("M-<Tab>"     , nextWindow)
  , ("M-S-<Tab>"   , prevWindow)
  , ("M-h"         , windows $ W.swapUp   . W.focusUp)
  , ("M-l"         , windows $ W.swapDown . W.focusDown)
  , ("M-t"         , withFocused $ windows . W.sink)
  , ("M-q"         , broadcastMessage ReleaseResources
                     >> restart "xmonad" True)
  , ("M-S-q"       , io exitSuccess)
  , ("M-S-b"       , sendMessage AvoidFloatToggle)
  , ("M-s p"       , unGrab >> powerMenu)


  -- window manipulation
  , ("M-w g"       , gotoMenuArgs  $ dmenuFlags ++ [ "-p", "Go to window:  "])
  , ("M-w b"       , bringMenuArgs $ dmenuFlags ++ [ "-p", "Bring window:  " ])


  -- SESSION --
  , ("M-s l"       , spawn screensaver)
  , ("M-s q"       , io exitSuccess)
  , ("M-s r"       , broadcastMessage ReleaseResources
                     >> restart "xmonad" True)

  -- APPLICATIONS --
  , ("M-x M-c"     , kill)


  -- TEST
  , ("M-`"  , scratchpadSpawnActionTerminal (term options))
  -- /TEST

  -- media keys
  , ("<XF86AudioPlay>",        spawn "playerctl play-pause")
  , ("<XF86AudioStop>",        spawn "playerctl stop")
  , ("<XF86AudioNext>",        spawn "playerctl next")
  , ("<XF86AudioPrev>",        spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
  , ("<XF86AudioMute>",        spawn "pactl set-sink-mute 0 toggle")
  ] ++
  -- standard jumping around workspaces etc.
  [ (m ++ k, windows $ f w)
  | (w, k) <- zip (XMonad.workspaces c) (spaces options)
  , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  ] ++
  -- search engine submap
  [ ("M-/  " ++ k, S.selectSearch f)       | (k,f) <- searchList ] ++
  [ ("M-S-/" ++ k, S.promptSearch P.def f) | (k,f) <- searchList ] -- FIXME: not appearing?
  -- @end keys
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp
        dmenuFlags      = [ "-fn", "Fira Sans-12"
                          , "-nb", "#2e3440"
                          , "-nf", "#d3dee9"
                          , "-sb", "#d08770"
                          , "-sf", "#2e3440"
                          , "-h" , "52"
                          , "-y" , "0"
                          ]

-- search engine submap, starts with M-s (selected) and M-S-s (prompt)
searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.duckduckgo)
             , ("h", S.hoogle)
             , ("w", S.wikipedia)]


-- Non-numeric num pad keys, sorted by number
numPadKeys :: [KeySym]
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]

------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events
mouseBindings' :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1, flexible linear scale
    [ ((modm, button1), \w -> focus w >> F.mouseWindow F.discrete w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
