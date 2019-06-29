-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.Events where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (join)
import qualified XMonad.StackSet as W


import XMonad

eventLogHook :: X ()
eventLogHook = do
  winset <- gets windowset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = " [" ++ ws ++ "] "
          | otherwise    = "  " ++ ws ++ "  "
        sort' = sortBy (compare `on` (!! 0))
