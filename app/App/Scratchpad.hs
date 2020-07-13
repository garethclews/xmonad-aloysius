-- | XMonad-aloysius scratchpad settings

module App.Scratchpad where

import           XMonad
import           XMonad.Util.Scratchpad

import qualified XMonad.StackSet               as W


manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
 where
  h = 1.00     -- height
  w = 0.29     -- width
  t = 0.00     -- distance from top
  l = 0.00     -- distance from left
