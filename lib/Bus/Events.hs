-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.Events where

import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Control.Monad                  ( join )

-- import           DBus
-- import           DBus.Client

import           XMonad
import qualified XMonad.StackSet               as W

import           Theme.ChosenTheme


-- Useful information for formatting
  -- %{T3} changes font to bold in polybar
  -- %{T-} resets it back to font-0
  -- this module then depends on +THEME+

logHook' :: X ()
logHook' = do
  winset <- gets windowset

  -- focussed app
  let currWin = head . W.index $ winset
  apStr <- filter (/= '"') . show <$> runQuery title currWin
  -- TODO:
  -- this will be used to remove notifications on entry into the app
  -- current plans are just for discord and geary and we need to abstract the
  -- tray functionality out into another file

  -- 'system tray'
  let tray = email ++ discord
       where
        email   = "\xf0e0"
        discord = "\xf086"
  -- io $ appendFile "/tmp/xmonad-tray" (tray ++ "\n")
  -- operations should be like this:
  -- 1. both are set to no notification
  -- 2. on the event that a org.freedesktop.Notification event for the apps
  --    specified is received then the icon for that app changes
  --    to the notification colour
  -- 3. on the event that a window changes we check to see if it has the correct name
  --    and if it does we revert the icon colour for that app to default


  -- workspaces
  let currWs = W.currentTag winset
  -- blocking named scratchpad appearing
  let wss = filter (/= "NSP") $ map W.tag $ W.workspaces winset
  let wsStr  = join $ map (fmt currWs) $ sort' wss

  -- layout
  let currLt = description . W.layout . W.workspace . W.current $ winset
  let ltStr  = layoutParse currLt

  -- fifo
  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")
  io $ appendFile "/tmp/xmonad-layout" (ltStr ++ "\n")


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
