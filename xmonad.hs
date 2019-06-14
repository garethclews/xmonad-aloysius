{-
  yay, xmonad

  inspired by:
    https://blog.pclewis.com/2016/03/19/xmonad-spacemacs-style.html
    https://git.sr.ht/~ben/cfg/tree/master/xmonad.hs
-}

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CopyWindow
import XMonad.Layout.WindowNavigation
import XMonad.Actions.WindowNavigation
import XMonad.Layout.Combo
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Layout.NoBorders
import XMonad.Actions.FloatKeys
import XMonad.Util.Replace
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.Master
import XMonad.Layout.Dishes
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Column
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Actions.ShowText
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.AvoidFloats
import System.IO (hClose, hFlush, Handle)
import Data.Maybe (fromMaybe, fromJust)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- personal data structures
import AOptions
import Aliases
import Dracula
import Layout

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
        logHook            = logs options,
        startupHook        = starts options
}



-- Keymaps --
keyMapDoc :: String -> X Handle
keyMapDoc name = do
  -- focused screen location/size
  r <- withWindowSet $ return . screenRect . W.screenDetail . W.current

  handle <- spawnPipe $ unwords [ "~/.xmonad/showHintForKeymap.sh"
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
  , ("/", spawn menu)
  ]
  where focus :: String -> X ()
        focus w = spawn ("wmctrl -a " ++ w)
        menu = dmenu

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
  where setContext = spawn ("~/.xmonad/sshot-context.sh")
        takeShot a = spawn ("~/.scripts/screenshot")
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
  ]
  where role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    replace
    config <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) defaults
    xmonad $ ewmh config `additionalKeys` ([((m .|. mask options, k), windows $ f i)
                                           | (i, k) <- zip (spaces options) [xK_1 .. xK_9]
                                           , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])

