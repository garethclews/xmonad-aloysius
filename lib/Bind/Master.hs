-- |

module Bind.Master where

import System.Exit

import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll

import XMonad.Hooks.ManageDocks (ToggleStruts(..))

import XMonad.Layout.AvoidFloats

import XMonad.Prompt.XMonad

import XMonad.Util.Scratchpad
import XMonad.Util.Ungrab

import qualified Data.Map                          as M
import qualified XMonad.Actions.FlexibleManipulate as F
import qualified XMonad.Actions.Search             as S
import qualified XMonad.StackSet                   as W

-- local
import App.Alias
import App.Launcher
import Bind.Util  -- replaces EZConfig, adds <S>
import Config.Options
import Theme.ChosenTheme


-- Keymaps ----------------------------------------------------------------------

-- default keymap
defaultKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
defaultKeys c = mkKeymap c $
  [ ("<S> <Return>"  , spawn (term options))
  , ("<S> <Space>"   , sendMessage NextLayout)
  , ("<S> <Tab>"     , windows W.focusDown)
  , ("<S> S-<Tab>"   , windows W.focusUp)
  , ("<S> p"         , spawn appLauncher)
  , ("<S> `"         , scratchpadSpawnActionCustom scratch)

  -- APPLICATIONS --
  , ("<S> a k"       , kill1)
  , ("<S> a f"       , spawn browser)
  , ("<S> a e"       , spawn code)


  -- window manipulation
  , ("<S> w g"       , gotoMenuArgs  $ dmenuTheme base12 "Go to window:  ")
  , ("<S> w b"       , bringMenuArgs $ dmenuTheme base12 "Bring<XF86AudioNext>"       , spawn "playerctl next")
  , ("<XF86AudioPrev>"       , spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
  , ("<XF86AudioMute>"       , spawn "pactl set-sink-mute 0 toggle")
  ] ++
  -- search engine submap
  [ ("<S> / s " ++ k, S.selectSearch f)              | (k,f) <- searchList ] ++
  [ ("<S> / p " ++ k, S.promptSearch promptConfig f) | (k,f) <- searchList ] ++
  -- standard jumping around workspaces etc.
  [ (m ++ k, windows $ f w)
  | (w, k) <- zip (XMonad.workspaces c) (spaces options)
  , (m, f) <- [("<S> ", W.greedyView), ("<S> S-", W.shift)]
  ]
  -- @end keys

-- Menu for less common actions
actions :: [ (String, X ()) ]
actions = [ ("increaseM"   , sendMessage (IncMasterN 1))
          , ("decreaseM"   , sendMessage (IncMasterN (-1)))
          , ("toggleStruts", sendMessage ToggleStruts)
          , ("screensaver" , spawn screensaver)
          , ("kill"        , kill1)
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
             , xK_KP_Insert
             ]

------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events
mouseBindings' :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1, flexible linear scale
    [ ((mod4Mask, button1), \w -> focus w >> F.mouseWindow F.discrete w)
    -- mod-button2, Raise the window to the top of the stack
    , ((mod4Mask, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
