-- | XMonad-aloysius scratchpad settings

module App.Scratchpad where

import           XMonad
import           XMonad.Util.Scratchpad

import qualified XMonad.StackSet               as W


manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
 where
  h = 0.3      -- height: 20%
  w = 1.0      -- width
  t = 1.0 - h  -- distance from top
  l = 1.0 - w  -- distance from left
