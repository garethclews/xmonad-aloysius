-- | XMonad-Aloysius setup

module Config.Options where


import           XMonad

import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Decoration
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
  { term   :: String
  , ffm    :: Bool
  , mask   :: KeyMask
  , spaces :: [String]
  , starts :: X ()
  }


options :: Options
options = Options
  { term   = tty
  , ffm    = True
  , mask   = mod4Mask
  , spaces = wsList
  , starts = setWMName "XMonad"
             >> spawnOnce touchEvents
             >> spawnOnce wallpaper
             >> spawnOnce cursor
             >> spawnOnce panel
             >> spawnOnce lang
             >> spawnOnce energyStar
             >> spawnOnce settingsd
  }


-- Theming related options ------------------------------------------------------
theme :: XMTheme
theme = XMTheme { highlight  = base00
                , background = base00
                , foreground = base04
                , focused    = basebg
                , unfocused  = basebg
                , border     = 0
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
  [ "-fn"
  , sansserif'
  , "-nb"
  , basebg
  , "-nf"
  , basefg
  , "-sf"
  , base00
  , "-sb"
  , colour
  , "-h"
  , "52"
  , "-p"
  , s
  ]


tabTheme :: Theme
tabTheme = def { activeColor         = base03
               , activeBorderColor   = base03
               , activeTextColor     = basefg
               , inactiveColor       = base02
               , inactiveBorderColor = base01
               , inactiveTextColor   = base00
               , urgentColor         = basebg
               , urgentBorderColor   = basebg
               , urgentTextColor     = base12
               , fontName            = monospace
               , decoHeight          = 52
               }


decoTheme :: Theme
decoTheme = def
  { activeColor         = basebg
  , activeBorderColor   = basebg
  , activeTextColor     = basefg
  , inactiveColor       = basebg
  , inactiveBorderColor = basebg
  , inactiveTextColor   = basebg
  , urgentColor         = basebg
  , urgentBorderColor   = basebg
  , urgentTextColor     = base12
  , fontName            = altsans
  , windowTitleAddons   = [ ("\xf111", AlignRightOffset 24)
                          , ("\xf111", AlignRightOffset 54)
                          , ("\xf111", AlignRightOffset 84)
                          ]
  , decoHeight          = 52
  }


emptyTheme :: Theme
emptyTheme = def { decoHeight = 0, decoWidth = 0 }


sideDecoTheme :: Theme
sideDecoTheme = def { activeColor         = basebg
                    , activeBorderColor   = basebg
                    , activeTextColor     = basefg
                    , inactiveColor       = basebg
                    , inactiveBorderColor = basebg
                    , inactiveTextColor   = basebg
                    , urgentColor         = basebg
                    , urgentBorderColor   = basebg
                    , urgentTextColor     = base12
                    , fontName            = sansserif
                    , decoWidth           = 52
                    }
