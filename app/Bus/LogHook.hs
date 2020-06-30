-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.LogHook
  ( logHooker
  )
where

import           Control.Monad                  ( forM_ )

import           XMonad
import qualified XMonad.StackSet               as W

import           Theme.ChosenTheme


-- Useful information for formatting
  -- %{T3} changes font to bold in polybar
  -- %{T-} resets it back to font-0
  -- this module then depends on +THEME+

-- could this also support a pop up panel?
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Loggers-NamedScratchpad.html


-- Supporting functions --------------------------------------------------------
mkLayoutStr :: String -> String -> String -> String
mkLayoutStr colour logo rep =
  concat ["%{T2}%{F", colour, "} ", logo, "%{T-}%{F", basefg, "} ", rep]


countWindows :: W.StackSet i l a s sd -> Int
countWindows wins = case W.stack . W.workspace . W.current $ wins of
  Nothing -> (-1)
  Just s  -> length $ W.up s ++ W.down s


layoutParse :: String -> String
layoutParse s | s == "Three Columns"    = mkLayoutStr base13 "+|+" "TCM "
              | s == "Binary Partition" = mkLayoutStr base13 "||+" "BSP "
              | s == "Tall"             = mkLayoutStr base13 "|||" "Tall"
              | s == "Tabbed"           = mkLayoutStr base13 "___" "Tab "
              | s == "Float"            = mkLayoutStr base13 "+++" "FLT "
              | s == "Fullscreen"       = mkLayoutStr base13 "| |" "Full"
              | otherwise               = s -- fallback for changes in C.Layout


write :: (String, String) -> X ()
write (x, y) = io $ appendFile x y


fmt :: String -> String
fmt = mkLayoutStr base11 "\xf041"  -- map location marker


-- Hook ------------------------------------------------------------------------
logHooker :: X ()
logHooker = do
  -- FIXME: can this be less onerous?
  winset <- gets windowset

-- ^ workspaces list
  let
    currWs = W.currentTag winset

-- ^ output string based on workspaces list
    wsStr  = fmt currWs

-- ^ current layout
    ltStr =
      layoutParse . description . W.layout . W.workspace . W.current $ winset

-- ^ strings for window operations
    copyStr = case countWindows winset of
      (-1) -> noneCommand
      _    -> copyCommand

    fullStr = case countWindows winset of
      (-1) -> noneCommand
      _    -> fullCommand

    killStr = case countWindows winset of
      (-1) -> noneCommand
      _    -> killCommand

-- pushing logs to pipes, note all files are FIFO specials
-- done this way to 'future-proof' against any stupid ideas I have
  forM_
    [ ("/tmp/xmonad-wspace", wsStr ++ "\n")
    , ("/tmp/xmonad-layout", ltStr ++ "\n")
    , ("/tmp/xmonad-copy"  , copyStr ++ "\n")
    , ("/tmp/xmonad-full"  , fullStr ++ "\n")
    , ("/tmp/xmonad-kill"  , killStr ++ "\n")
    ]
    write


-- Strings ----------------------------------------------------------------------
killCommand :: String
killCommand =
  concat ["%{A1:xdotool key Super a q:}%{F", basebg, "}\xf192%{F-}%{A}"]

copyCommand :: String
copyCommand =
  concat ["%{A1:xdotool key Super w c:}%{F", basebg, "}\xf004%{F-}%{A}"]

fullCommand :: String
fullCommand = concat
  ["%{A1:xdotool key Super l 1 Super w c:}%{F", basebg, "}\xf005%{F-}%{A}"]

noneCommand :: String
noneCommand = concat ["%{F", base11, "}\xf004%{F-}"]
