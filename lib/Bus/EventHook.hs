-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.EventHook
  ( eventer
  )
where

import           Data.Monoid
import           XMonad
import           Theme.ChosenTheme


-- event hook manager and the handler for specific events
eventer :: Event -> X All
eventer e = handle e >> return (All True)


handle :: Event -> X ()
handle KeyEvent { ev_event_type = t, ev_state = _, ev_keycode = _ }
  | t == 2    = io $ appendFile "/tmp/xmonad-events" fullDot
  | otherwise = io $ appendFile "/tmp/xmonad-events" voidDot
handle _ = return ()


-- pretty content ---------------------------------------------------------------
fullDot :: String
fullDot = "%{F" ++ base14 ++ "}\xf111%{F-}\n"

voidDot :: String
voidDot = "\xf111\n"
