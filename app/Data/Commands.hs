-- | Commands to include in prompts
-- TODO

module Data.Commands where

import XMonad
import XMonad.Actions.Commands

commands :: X [(String, X ())]
commands = defaultCommands
