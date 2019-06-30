-- |

module Bind.Master where

import System.IO (hClose, hFlush, Handle)
import System.Exit

import XMonad

import XMonad.Actions.Commands
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS

import XMonad.Layout.AvoidFloats
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns

-- import XMonad.Prompt  -- build custom prompt to help find keys
-- https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Prompt.html

import XMonad.Util.Dzen
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- local
import App.Alias
import Config.Options
import Data.Commands
import Theme.Nord


-- Keymaps --
keyMapDoc :: String -> X Handle
keyMapDoc name = do
  -- focused screen location/size
  r <- withWindowSet $ return . screenRect . W.screenDetail . W.current

  handle <- spawnPipe "echo"
  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  io $ appendFile "/tmp/xmonad-mode" name
  submap $ mkKeymap c m


-- media keys
--mediaKeys =
--  [ ("XF86AudioPlay",       spawn "playerctl play-pause")
--  , ("XF86AudioStop",        spawn "playerctl stop")
--  , ("XF86AudioNext",        spawn "playerctl next")
--  , ("XF86AudioPrev",        spawn "playerctl previous")
-- , ("XF86AudioLowerVolume", spawn "pactl set-sink-volume 0 -5%")
--  , ("XF86AudioRaiseVolume", spawn "pactl set-sink-volume 0 +5%")
--  , ("XF86AudioMute",        spawn "pactl")
--  ]

-- window management
windowKeys =
  [ ("s",        sendMessage Shrink)
  , ("e",        sendMessage Expand)
  -- TODO bind escape to 'leave submap'
  ] -- ++ mediaKeys FIXME

layoutKeys =
  [ ("1",        rescreen)
  , ("2",        layoutSplitScreen 2 $ TwoPane (3/100) (1/2))
  , ("3",        layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2))
  , ("4",        layoutSplitScreen 4   Grid)
  , ("5",        layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2))
  -- TODO bind escape to 'leave submap'
  ] -- ++ mediaKeys FIXME

resizeKeys =
  [ ("h",        sendMessage Shrink)
  , ("l",        sendMessage Expand)
  , ("k",        incMaster)
  , ("j",        decMaster)
  -- TODO bind escape to 'leave submap'
  ] -- ++ mediaKeys FIXME
  where incMaster       = sendMessage (IncMasterN 1)
        decMaster       = sendMessage (IncMasterN (-1))

-- default keymap
defaultKeys c = mkKeymap c $
  [ ("M-<Return>", spawn (term options))
  , ("M-p"       , spawn dmenu)
  , ("M-<Space>" , sendMessage NextLayout)
  , ("M-<Tab>"   , nextWindow)
  , ("M-S-<Tab>" , prevWindow)
  , ("M-t"       , withFocused $ windows . W.sink)
  , ("M-q"       , broadcastMessage ReleaseResources
                   >> restart "xmonad" True)
  , ("M-S-q"     , io (exitWith ExitSuccess))
  -- , ("M-S-q"       , spawn "~/.scripts/polybar/xmonad-power")
  , ("M-S-b"     , sendMessage AvoidFloatToggle)

  -- key combinations
  -- CONTAINERS --
  , ("M-w <Space>", toSubmap c "window"        windowKeys)
  , ("M-w l"      , toSubmap c "window-layout" layoutKeys)
  , ("M-w r"      , toSubmap c "window-resize" resizeKeys)
  , ("M-w <Esc>"  , toSubmap c "default" [])
  , ("M-w ?"      , spawn "echo 'TO DO' | dzen2")
  , ("M-]"        , dzenConfig (timeout 10
                                >=> font "Fira Sans:size=14"
                                >=> onCurr xScreen
                                >=> onCurr (hCenter 500)
                                >=> onCurr (vCenter 300)
                                >=> fgColor nord04
                                >=> bgColor nord00)
                                "Hi Aloysius, I'm DZen")

  -- SESSION --
  , ("M-s l"      , spawn "i3lock-fancy")
  , ("M-s q"      , io (exitWith ExitSuccess))
  , ("M-s r"      , broadcastMessage ReleaseResources
                    >> restart "xmonad" True)

  -- APPLICATIONS --
  , ("M-x c"      , kill)


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
  ]
  -- ++ mediaKeys FIXME
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp


-- Non-numeric num pad keys, sorted by number
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]

------------------------------------------------------------------------

-- Mouse bindings: default actions bound to mouse events
mouseBindings' (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
