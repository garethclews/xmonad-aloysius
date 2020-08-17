-- |

module Bind.Master where

import           System.Exit

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WithAll
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..) )

import           XMonad.Layout.LayoutCombinators
                                                ( JumpToLayout(..) )

import           XMonad.Prompt.XMonad

import           XMonad.Util.Scratchpad
import           XMonad.Util.Ungrab

import qualified Data.Map                      as M
import qualified XMonad.Actions.FlexibleManipulate
                                               as F
import qualified XMonad.Actions.Search         as S
import qualified XMonad.StackSet               as W

-- local
import           App.Alias
import           App.Launcher
import           Bind.Util  -- replaces EZConfig, adds <S>
import           Config.Options
import           Theme.ChosenTheme


-- Keymaps ----------------------------------------------------------------------

-- TODO: investigate minimize:
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-Minimize.html
-- not interested in maximise

-- see about windowmenu
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-WindowMenu.html




-- @start keys
defaultKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
defaultKeys c =
  mkKeymap c
    $  [ ( "<S> <Return>"
         , spawn $ term options
         )   -- general binds
       , ("<S> <Space>", sendMessage NextLayout)
       , ("<S> <Tab>"  , windows W.focusDown)
       , ("<S> S-<Tab>", windows W.focusUp)
       , ("<S> p"      , spawn appLauncher)
       , ("<S> `"      , scratchpadSpawnActionCustom scratch)
       , ( "<S> a q"
         , kill1
         )
       -- application launchers
       , ("<S> a w", spawn browser)
       , ( "<S> a e"
         , spawn code
         )
        -- window manipulation
       , ("<S> w g", gotoMenuArgs $ dmenuTheme base10 "Go to window:  ")
       , ("<S> w b", bringMenuArgs $ dmenuTheme base15 "Bring window:  ")
       , ("<S> w h"      , sendMessage Shrink)
       , ("<S> w l"      , sendMessage Expand)
       , ("<S> w ."      , sendMessage $ IncMasterN 1)
       , ("<S> w ,"      , sendMessage $ IncMasterN (-1))
       , ("<S> w m"      , windows W.focusMaster)
       , ("<S> w <Left>" , windows $ W.swapUp . W.focusUp)
       , ("<S> w <Right>", windows $ W.swapDown . W.focusDown)
       , ("<S> w s"      , withFocused $ windows . W.sink)
       , ("<S> w t", sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
       , ("<S> w c"      , windows copyToAll)
       , ("<S> w k"      , killAllOtherCopies)
       , ("<S> w <Down>" , sinkAll)
       , ( "<S> q l"
         , spawn screensaver
         )
       -- layout manipulation
       , ("<S> l 1", sendMessage $ JumpToLayout "Fullscreen")
       , ("<S> l 2", sendMessage $ JumpToLayout "Tall")
       , ( "<S> l 3"
         , sendMessage $ JumpToLayout "Tabbed"
         )
       -- session
       , ("<S> q c", spawn "$HOME/.scripts/caffeine")
       , ("<S> q r", broadcastMessage ReleaseResources >> restart "xmonad" True)
       , ("<S> q q", io exitSuccess)
       , ("<S> q m", unGrab >> powerMenu)
       , ( "<S> / /"
         , xmonadPromptC actions promptConfig
         )
       -- searches
       , ( "<XF86AudioPlay>"
         , spawn "playerctl play-pause"
         )
       -- media keys
       , ("<XF86AudioStop>"       , spawn "playerctl stop")
       , ("<XF86AudioNext>"       , spawn "playerctl next")
       , ("<XF86AudioPrev>"       , spawn "playerctl previous")
       , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
       , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
       , ("<XF86AudioMute>"       , spawn "pactl set-sink-mute 0 toggle")
       ]
    ++ [ ("<S> / s " ++ k, S.selectSearch f) | (k, f) <- searchList ]  -- search options
    ++ [ ("<S> / p " ++ k, S.promptSearch promptConfig f)
       | (k, f) <- searchList
       ]
    ++ [ (m ++ k, windows $ f w)  -- navigation
       | (w, k) <- zip (XMonad.workspaces c) (spaces options)
       , (m, f) <- [("<S> ", W.greedyView), ("<S> S-", W.shift)]
       ]
-- @end keys

-- Menu for less common actions or those without media keys
actions :: [(String, X ())]
actions =
  [ ("inc-win", sendMessage (IncMasterN 1))
  , ("dec-win", sendMessage (IncMasterN (-1)))
  , ("struts", sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
  , ("lock"   , spawn screensaver)
  , ("kill"   , kill1)
  , ("mplay"  , spawn "playerctl play-pause")
  , ("mpause" , spawn "playerctl play-pause")
  , ("mstop"  , spawn "playerctl stop")
  , ("mnext"  , spawn "playerctl next")
  , ("mprev"  , spawn "playerctl previous")
  , ("mdown"  , spawn "pactl set-sink-volume 0 -5%")
  , ("mup"    , spawn "pactl set-sink-volume 0 +5%")
  , ("mmute", spawn "pactl set-sink-mute 0 toggle")
  ]


-- search engine submap
searchList :: [(String, S.SearchEngine)]
searchList = [("g", S.duckduckgo), ("h", S.hoogle), ("w", S.wikipedia)]


-- Non-numeric num pad keys, sorted by number
numPadKeys :: [KeySym]
numPadKeys =
  [ xK_KP_End
  , xK_KP_Down
  , xK_KP_Page_Down -- 1, 2, 3
  , xK_KP_Left
  , xK_KP_Begin
  , xK_KP_Right     -- 4, 5, 6
  , xK_KP_Home
  , xK_KP_Up
  , xK_KP_Page_Up   -- 7, 8, 9
  , xK_KP_Insert
  ]


------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events
mouseBindings' :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' XConfig { XMonad.modMask = modm } = M.fromList
    -- mod-button1, flexible linear scale
  [ ( (modm, button1)
    , \w -> focus w >> F.mouseWindow F.discrete w
    )
    -- mod-button4, Raise the window to the top of the stack
  , ( (modm, button4)
    , \w -> focus w >> windows W.shiftMaster
    )
    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- and middle click (button3)
  ]



-- Support functions ------------------------------------------------------------
-- showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
-- showKeybindings x = addName "Show Keybindings" $ io $ do
--   h <- spawnPipe "zenity --text-info --font=terminus"
--   hPutStr h (unlines $ showKm x)
--   hClose h
--   return ()
