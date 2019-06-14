-- | XMonad-Aloysius key config, including spacenads

module Keys where

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

import AOptions
import Aliases

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
    , ("C-M1-l",       spawn "xscreensaver-command -lock") -- Screen Lock
    , ("M-w",          toSubmap c "focusKeymap" focusKeymap) -- Focus
    , ("M-a",          toSubmap c "masterKeymap" masterKeymap) -- Master
    , ("M-=",          toSubmap c "screenKeymap" screenKeymap) -- Screen
    , ("M-s",          toSubmap c "shotKeymap" shotKeymap) -- Screen Shot
    , ("M-S-/",        toSubmap c "mainKeymap" []) -- Main Menu
    , ("M-S-b",        sendMessage AvoidFloatToggle)
    ]
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp


    --  Reset the layouts on the current workspace to default
    -- , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    -- , ((modm,               xK_r     ), refresh)
    -- swap clip/primary
    --, ((modm .|. shiftMask, xK_v     ), spawn "/home/pcl/swapbuf.sh")
    -- copy window
    --, ((modm .|. controlMask, xK_t ), windows copyToAll) -- @@ Make focused window always visible
    --, ((modm .|. shiftMask, xK_t ),  killAllOtherCopies) -- @@ Toggle window state back
    -- 4 screens
    --, ((modm,               xK_grave ), layoutSplitScreen 4 Grid)
    --, ((modm .|. mod1Mask,  xK_grave ), layoutSplitScreen 2 $ Mirror $ TwoPane (3/100) (1/2))
    --, ((modm .|. shiftMask, xK_grave ), rescreen)

    --, ((modm ,              xK_equal ), layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2))
    --, ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 3 $ Mirror $ Tall 1 (3/100) (3/4))
    --, ((modm .|. shiftMask,  xK_equal ), layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2))
    -- , ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 5 $ Mirror $ ThreeColMid 1 (3/100) (1/2))

-- Non-numeric num pad keys, sorted by number
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]
