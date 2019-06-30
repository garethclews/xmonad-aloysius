-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.Events where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (join)

import XMonad
import qualified XMonad.StackSet as W

import Theme.Nord


logHook' :: X ()
logHook' = do
  winset <- gets windowset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")

  where fmt currWs ws
          -- %{T3} changes font to bold in polybar
          -- %{T-} resets it back to font-0
          -- NOTE: Foreground colours also edited here
          -- this block then depends on +THEME+
          | currWs == ws = " %{F"++nord00++"}%{T3}[" ++ ws ++ "]%{T-}%{F-} "
          | otherwise    = "  %{F"++nord10++"}%{T4}" ++ ws ++ "%{T-}%{F-}  "
        sort' = sortBy (compare `on` (!! 0))
