-- | XMonad-Aloysius setup

module Config.Options where

import           Data.Monoid

import           XMonad

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

import           XMonad.Layout.Tabbed

import           XMonad.Util.Font
import           XMonad.Util.SpawnOnce

import qualified XMonad.Prompt                 as P

-- import Theme.Dracula
import           App.Alias
import           Config.Projects
import           Theme.ChosenTheme

-- data declarations ------------------------------------------------------------
-- general XMonad theme settings
data XMTheme = XMTheme
  { foreground :: String
  , background :: String
  , highlight  :: String
  , focused    :: String
  , unfocused  :: String
  , border     :: Dimension
  }


-- preferences ------------------------------------------------------------------
data Options = Options
  { term       :: String
  , ffm        :: Bool
  , mask       :: KeyMask
  , spaces     :: [String]
  , events     :: Event  -> X All
  , starts     :: X ()
  }

options :: Options
options = Options
  { term   = tty
  , ffm    = True
  , mask   = mod4Mask
  , spaces = wsList
  , events = ewmhDesktopsEventHook
  , starts = ewmhDesktopsStartup
             >> setWMName "XMonad"
             >> spawnOnce audioSink
             >> spawnOnce panel
             >> spawnOnce numlock
             >> spawnOnce wallpaper
             >> spawnOnce compositor
             >> spawnOnce cursor
             >> spawnOnce lang
             >> spawnOnce energyStar
             >> spawnOnce xresource
  }


-- Theming related options ------------------------------------------------------
theme :: XMTheme
theme = XMTheme { highlight  = base00
                , background = base00
                , foreground = base04
                , focused    = basebg
                , unfocused  = basebg
                , border     = 1
                }


promptConfig :: P.XPConfig
promptConfig = P.def { P.fgColor           = base04
                     , P.bgColor           = basebg
                     , P.font              = sansserif
                     , P.promptBorderWidth = 0
                     , P.height            = 52
                     , P.defaultText       = ""
                     , P.historySize       = 0
                     , P.maxComplRows      = Just 0
                     , P.position          = P.Top
                     }


dmenuTheme :: String -> String -> [String]
dmenuTheme colour s =
  [ "-fn", sansserif'
  , "-nb", basebg
  , "-nf", base04
  , "-sf", base00
  , "-sb", colour
  , "-h", "52"
  , "-p", s
  ]


tabTheme = def { activeColor         = base00
               , activeBorderColor   = base00
               , activeTextColor     = base06
               , inactiveColor       = basebg
               , inactiveBorderColor = basebg
               , inactiveTextColor   = base03
               , urgentColor         = basebg
               , urgentBorderColor   = basebg
               , urgentTextColor     = base12
               , fontName            = sansserif
               , decoHeight          = 52
               }


decoTheme = def { activeColor         = base00
                , activeBorderColor   = basebg
                , activeTextColor     = base06
                , inactiveColor       = basebg
                , inactiveBorderColor = basebg
                , inactiveTextColor   = base03
                , urgentColor         = basebg
                , urgentBorderColor   = basebg
                , urgentTextColor     = base12
                , fontName            = monospace
                , windowTitleAddons   = [("\xf004", AlignRightOffset 24)]
                , decoHeight          = 40
                , decoWidth           = 2560
                }
