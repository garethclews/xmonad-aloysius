-- |

module Bind.Master where

import System.Exit

import XMonad

import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll

import XMonad.Layout.AvoidFloats

import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Ungrab

import qualified Data.Map                          as M
import qualified XMonad.Actions.FlexibleManipulate as F
import qualified XMonad.Actions.Search             as S
import qualified XMonad.StackSet                   as W

-- local
import App.Alias
import App.Launcher
import Config.Options
import Theme.ChosenTheme


-- Keymaps ----------------------------------------------------------------------

-- default keymap
-- TODO: implement leader type key usage

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
  , ("M-`"         , scratchpadSpawnActionCustom scratch)


  -- window manipulation
  , ("M-w g"       , gotoMenuArgs  $ dmenuTheme base12 "Go to window:  ")
  , ("M-w b"       , bringMenuArgs $ dmenuTheme base12 "Bring window:  ")
  , ("M-w h"       , sendMessage Shrink)
  , ("M-w l"       , sendMessage Expand)
  , ("M-w."       , sendMessage $ IncMasterN 1)
  , ("M-w ,"       , sendMessage $ IncMasterN (-1))
  , ("M-w m"       , windows W.focusMaster)
  , ("M-w t"       , sinkAll)
  , ("M-w f"       , sendMessage AvoidFloatToggle)


  -- SESSION --
  , ("M-q l"       , spawn screensaver)
  , ("M-q r"       , broadcastMessage ReleaseResources
                     >> restart "xmonad" True)
  , ("M-q q"       , io exitSuccess)
  , ("M-q m"       , unGrab >> powerMenu)


  -- APPLICATIONS --
  , ("M-x c"       , kill)

  -- media keys
  , ("<XF86AudioPlay>"       , spawn "playerctl play-pause")
  , ("<XF86AudioStop>"       , spawn "playerctl stop")
  , ("<XF86AudioNext>"       , spawn "playerctl next")
  , ("<XF86AudioPrev>"       , spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
  , ("<XF86AudioMute>"       , spawn "pactl set-sink-mute 0 toggle")
  ] ++
  -- search engine submap
  [ ("M-/ s " ++ k, S.selectSearch f)              | (k,f) <- searchList ] ++
  [ ("M-/ p " ++ k, S.promptSearch promptConfig f) | (k,f) <- searchList ] ++
  -- standard jumping around workspaces etc.
  [ (m ++ k, windows $ f w)
  | (w, k) <- zip (XMonad.workspaces c) (spaces options)
  , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  ]
  -- @end keys
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp


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
