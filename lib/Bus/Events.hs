-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.Events where

import           Data.Function                  ( on )
import           Data.List                      ( sortBy )

import           XMonad
import           XMonad.Hooks.UrgencyHook       ( readUrgents )
import qualified XMonad.StackSet               as W

import           Theme.ChosenTheme


-- Useful information for formatting
  -- %{T3} changes font to bold in polybar
  -- %{T-} resets it back to font-0
  -- this module then depends on +THEME+

logHook' :: X ()
logHook' = do
  winset <- gets windowset

  -- workspaces
  let currWs = W.currentTag winset
  -- blocking named scratchpad appearing
  let wss    = filter (/= "NSP") $ W.tag <$> W.workspaces winset
  let wsStr  = fmt currWs =<< sort' wss

  -- layout
  let ltStr =
        layoutParse . description . W.layout . W.workspace . W.current $ winset

  urStr <- urgentParse <$> readUrgents

  -- fifo
  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")
  io $ appendFile "/tmp/xmonad-layout" (ltStr ++ "\n")
  io $ appendFile "/tmp/xmonad-tray" (urStr ++ "\n")


fmt :: String -> String -> String
fmt currWs ws
  | currWs == ws
  = " [%{F" ++ base06 ++ "}%{T1}" ++ ws ++ "%{T-}%{F" ++ base02 ++ "}] "
  | otherwise
  = "  " ++ ws ++ "  "

sort' :: Ord a => [[a]] -> [[a]]
sort' = sortBy (compare `on` (!! 0))


layoutParse :: String -> String
layoutParse s | s == "Three Columns"    = "%{T2}+|+%{T-} TCM "
              | s == "Binary Partition" = "%{T2}||+%{T-} BSP "
              | s == "Tall"             = "%{T2}|||%{T-} Tall"
              | s == "Tabbed"           = "%{T2}___%{T-} Tab "
              | s == "Float"            = "%{T2}+++%{T-} FLT "
              | s == "Fullscreen"       = "%{T2}| |%{T-} Full"
              | otherwise               = s -- fallback for changes in C.Layout


urgentParse :: [Window] -> String
urgentParse ws = "[ \xf0e0 \xf004 ]"
 where
  email _ = "mail"
  discord _ = "discord"
