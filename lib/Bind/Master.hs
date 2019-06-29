-- |

module Bind.Master where

import System.IO (hClose, hFlush, Handle)
import System.Exit

import XMonad

import XMonad.Actions.Submap

import XMonad.Layout.AvoidFloats
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns

import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- local
import App.Alias
import Config.Options


-- Keymaps --
keyMapDoc :: String -> X Handle
keyMapDoc name = do
  -- focused screen location/size
  r <- withWindowSet $ return . screenRect . W.screenDetail . W.current

  handle <- spawnPipe $ unwords [ "~/.xmonad/hints"
                                , name
                                , show (rect_x r)
                                , show (rect_y r)
                                , show (rect_width r)
                                , show (rect_height r)
                                , "pink"       -- key color
                                , "white"        -- cmd color
                                , "Fira\\ Mono" -- font
                                , "12"           -- line height
                                ]

  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe

-- Note: Formatting is important for script
focusKeymap = -- Focus
  [ ("f", focus "firefox") -- vimperator
  , ("e", focus "emacs") -- Emacs
  , ("m", windows W.focusMaster) -- Focus Master
  , ("/", spawn dmenu)
  ]
  where focus :: String -> X ()
        focus w = spawn ("wmctrl -a " ++ w)

masterKeymap = -- Master Window
  [ ("f",   windows W.focusMaster) -- Focus
  , ("s",   windows W.swapMaster) -- Swap
  , ("h",   sendMessage Shrink) -- Shrink
  , ("l",   sendMessage Expand) -- Expand
  , ("k",   incMaster) -- Inc
  , ("j",   decMaster) -- Dec
  ]
  where incMaster       = sendMessage (IncMasterN 1)
        decMaster       = sendMessage (IncMasterN (-1))

screenKeymap = -- Screen
  [ ("0", rescreen)
  , ("2", layoutSplitScreen 2 $ TwoPane (3/100) (1/2)) -- TwoPane
  , ("3", layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2)) -- ThreeColmid
  , ("4", layoutSplitScreen 4 Grid) -- Grid
  , ("5", layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2)) -- ThreeColmid
  ]

shotKeymap = -- Screen Shot
  [ ("c", setContext) -- Context
  , ("s", takeShot select) -- Select
  , ("w", takeShot currentWindow) -- Current Window
  , ("o", openDirectory)
  ]
  where setContext    = spawn ("~/.scripts/screenshot")
        takeShot a    = spawn ("~/.scripts/screenshot")
        openDirectory = spawn ("xdg-open ~/Pictures/screens/")
        select        = "-s"
        currentWindow = "-u"

mainKeymap c = mkKeymap c $
    [ ("M-S-<Return>", spawn (term options)) -- Terminal
    , ("M-p",          spawn dmenu) -- Dmenu
    , ("M-S-c",        kill)
    , ("M-<Space>",    sendMessage NextLayout) -- Next Layout
    , ("M-<Tab>",      nextWindow) -- Next Window
    , ("M-S-<Tab>",    prevWindow) -- Prev Window
    , ("M-M1-h",       sendMessage Shrink) -- Shrink
    , ("M-M1-l",       sendMessage Expand) -- Expand
    , ("M-t",          withFocused $ windows . W.sink) -- Sink
    , ("M-q",          spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ("M-S-q",        io $ exitWith ExitSuccess) -- Exit
    , ("C-S-l",        spawn "i3lock-fancy") -- Screen Lock
    , ("M-w",          toSubmap c "focusKeymap" focusKeymap) -- Focus
    , ("M-a",          toSubmap c "masterKeymap" masterKeymap) -- Master
    , ("M-=",          toSubmap c "screenKeymap" screenKeymap) -- Screen
    , ("M-s",          toSubmap c "shotKeymap" shotKeymap) -- Screen Shot
    , ("M-S-/",        toSubmap c "mainKeymap" []) -- Main Menu
    , ("M-S-b",        sendMessage AvoidFloatToggle)
    ]
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
