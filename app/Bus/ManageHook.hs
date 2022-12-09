-- | ManageHooks for xmonad-aloysius

module Bus.ManageHook
  ( manager
  ) where

import           Data.Ratio                     ( (%) )

import           XMonad

import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet               as W

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

-- look to incorporate this:
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-FadeWindows.html

-- manager :: ManageHook
-- manager =
--   composeOne
--       [ className =? "Places" -?> doFloat
--       , isDialog -?> doCenterFloat
--       , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
--       , isInProperty "_NET_WM_WINDOW_TYPE" splash -?> doCenterFloat
--       , transience
--       , pure True -?> insertPosition End Newer
--       ]
--     <+> manageDocks
--     <+> manageScratchpad
--  where
--   isRole = stringProperty "WM_WINDOW_ROLE"
--   splash = "_NET_WM_WINDOW_TYPE_SPLASH"

manager :: ManageHook
manager =
  composeOne
      [ className =? "Places" -?> doFloat
      , className =? "mpv" -?> doRectFloat
        (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
      , className =? "Firefox" <&&> resource =? "Toolkit" -?> doFloat
      , isDialog -?> doCenterFloat
      , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
      , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
        -?> doCenterFloat
      , transience
      , pure True -?> insertPosition End Newer
      ]
    <+> manageDocks
  where isRole = stringProperty "WM_WINDOW_ROLE"
