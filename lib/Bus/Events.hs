-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.Events where

import Data.List (sortBy, isInfixOf)
import Data.Function (on)
import Control.Monad (join)

import XMonad
import qualified XMonad.StackSet as W

import Theme.ChosenTheme


logHook' :: X ()
logHook' = do
  -- curLayout <- gets (description . W.layout . W.workspace . W.current $ windowset)
  -- curSpaces <- gets (description . W.workspaces . W.currentTag $ windowset)
  winset <- gets windowset

  -- workspaces
  let currWs = W.currentTag winset
  -- blocking named scratchpad appearing
  let wss = filter (/= "NSP") $ map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  -- layout
  let currLt = description . W.layout . W.workspace . W.current $ winset
  let ltStr  = layoutParse currLt

  -- fifo
  io $ appendFile "/tmp/xmonad-ws"     (wsStr ++ "\n")
  io $ appendFile "/tmp/xmonad-layout" (ltStr ++ "\n")
  where
    fmt currWs ws
      -- %{T3} changes font to bold in polybar
      -- %{T-} resets it back to font-0
      -- NOTE: Foreground colours also edited here
      -- this block then depends on +THEME+
      | currWs == ws = " [%{F"  ++ base06 ++ "}%{T3}" ++ ws ++ "%{T-}%{F" ++ base02 ++ "}] "
      | otherwise    = "  " ++ ws ++ "  "

    sort' = sortBy (compare `on` (!! 0))
    layoutParse s  -- 'pretty' printing
      | s == "Three Columns"    = "%{T2}+|+%{T-} TCM "
      | s == "Binary Partition" = "%{T2}||+%{T-} BSP "
      | s == "Tall"             = "%{T2}|||%{T-} Tall"
      | s == "Tabbed"           = "%{T2}___%{T-} Tab "
      | s == "Float"            = "%{T2}+++%{T-} FLT "
      | s == "Fullscreen"       = "%{T2}| |%{T-} Full"
      | otherwise               = s -- fallback for changes in C.Layout
