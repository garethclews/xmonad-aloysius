-- | XMonad-Aloysius event hook management
--
-- I'm only currently interested in key press events marking keyboard capture

module Bus.EventHook
  ( eventer
  )
where

import           Data.Monoid
import           XMonad
import qualified XMonad.StackSet               as W
import           Theme.ChosenTheme


-- event hook manager and the handler for specific events
eventer :: Event -> X All
eventer e = handle e >> return (All True)


handle :: Event -> X ()
handle KeyEvent { ev_event_type = t, ev_state = _, ev_keycode = _ }
  | t == 2    = io $ appendFile "/tmp/xmonad-events" fullDot
  | otherwise = io $ appendFile "/tmp/xmonad-events" voidDot

handle ClientMessageEvent { ev_window = w, ev_message_type = t, ev_data = d } =
  withWindowSet $ \s -> do
    let ws = W.workspaces s
    io $ appendFile "/tmp/xmonad-stack" (show d ++ "\n")
    return ()

handle _ = return ()

-- pretty content ---------------------------------------------------------------
fullDot :: String
fullDot = "%{F" ++ base14 ++ "}\xf111%{F-}\n"

voidDot :: String
voidDot = "\xf111\n"
