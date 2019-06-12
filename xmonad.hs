--------------------------------------------------------------------------------
{-
                       __  ____  __                       _
                       \ \/ /  \/  | ___  _ __   __ _  __| |
                        \  /| |\/| |/ _ \| '_ \ / _` |/ _` |
                        /  \| |  | | (_) | | | | (_| | (_| |
                       /_/\_\_|  |_|\___/|_| |_|\__,_|\__,_|
-}
---------------------------------------------------------------------------------


-- Imports ----------------------------------------------------------------------
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run        (spawnPipe)
import XMonad.Util.EZConfig   (additionalKeys)
import XMonad.Util.SpawnOnce

import Data.List
import qualified XMonad.StackSet as W

import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed

import XMonad.Actions.CycleWS (prevWS, nextWS)

import System.IO


-- named options ----------------------------------------------------------------

-- colours
normBord = "#44475a"
focdBord = "#bbe9fd"
fore     = "#dee3e0"
back     = "#282a36"
winType  = "#c678dd"

-- borders
borderWidth  = 1
normalBorder = normBord
focusBorder  = focdBord

-- programs
dmenu = "dmenu_run -b -i -fn 'xft:Fira Mono:pixelsize=16' -p 'Run: '"

-----------

myWorkspaces :: [String]
myWorkspaces = click $ [" 1 "," 2 "," 3 "," 4 "," 5 "]
  where click l = [ "^ca(1, xdotool key super+"
                    ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                    (i,ws) <- zip [1..] l,
                    let n = i]

-- keys
mKeys = [ ((modm, xK_p), spawn $ dmenu)
        , ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)
--      , ((modm                 .|. shiftMask, xK_z    ), spawn "slock"                 )
        , ((modm .|. controlMask              , xK_s    ), sendMessage  Arrange          )
        , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange        )
        , ((modm .|. controlMask              , xK_Left ), sendMessage (MoveLeft      10))
        , ((modm .|. controlMask              , xK_Right), sendMessage (MoveRight     10))
        , ((modm .|. controlMask              , xK_Down ), sendMessage (MoveDown      10))
        , ((modm .|. controlMask              , xK_Up   ), sendMessage (MoveUp        10))
        , ((modm                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  10))
        , ((modm                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 10))
        , ((modm                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  10))
        , ((modm                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    10))
        , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  10))
        , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 10))
        , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  10))
        , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    10))
        , ((modm, xK_KP_Add), sequence_ [ sendMessage (IncreaseLeft 10)
                    , sendMessage (IncreaseRight 10)
                    , sendMessage (IncreaseUp 10)
                    , sendMessage (IncreaseDown 10)
                    ])
        , ((modm, xK_KP_Subtract), sequence_ [ sendMessage (DecreaseLeft 10)
                         , sendMessage (DecreaseRight 10)
                         , sendMessage (DecreaseUp 10)
                         , sendMessage (DecreaseDown 10)
                         ])
    ] where modm = mod1Mask
                -- mod1Mask  -- prefer alt_l

startUp :: X()
startUp = do
    setWMName "xmonad"
    setWMName "xmonad"

logbar h = do
  dynamicLogWithPP $ tryPP h

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput             = hPutStrLn h
    , ppCurrent            = dzenColor (fore) (normBord) . pad
    , ppVisible            = dzenColor (fore) (back) . pad
    , ppHidden             = dzenColor (fore) (back) . pad
    , ppHiddenNoWindows    = dzenColor (fore) (back) . pad
    , ppUrgent             = dzenColor (fore) (focdBord) . pad
    , ppOrder              = \(ws:l:t:_) -> [ws,l]
    , ppSep                = ""
    , ppLayout             = dzenColor (fore) (winType) .
                ( \t -> case t of
                    "Spacing 2 ResizableTall" -> " " ++ i ++ "tile.xbm) TALL "
                    "Full" -> " " ++ i ++ "dice1.xbm) FULL "
                    "Circle" -> " " ++ i ++ "dice2.xbm) CIRC "
                    _ -> " " ++ i ++ "tile.xbm) TALL "
                )
    } where i = "^i(/home/aloysius/.icons/stlarch/"



-- layout --

resize = ResizableTall 1 (2/100) (1/2) []
full = noBorders (fullscreenFull Full)

-- useless gap --
layout = (gaps [(U, 42), (R, 10), (L, 10), (D, 32)] $
           avoidStruts (spacing 2 $ resize)) ||| Circle ||| full

------------

main = do
    bar <- spawnPipe panel
    info <- spawnPipe "conky -c ~/.conky/top.conky |\
                      \dzen2 -x 400 -y 10 -h 24 -w 2150 -p -ta r -e''"
    xmonad $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = windowArrange layout
        , startupHook = startUp
        , workspaces = myWorkspaces
        , terminal = "urxvt"
        , XMonad.borderWidth = 1
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        , logHook = logbar bar
        , modMask = mod1Mask
        } `additionalKeys` mKeys
        where panel = "dzen2 -ta l -p -w 400 -y 10 -x 10 -h 24 -e ''"
