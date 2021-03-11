-- | XMonad-Aloysius eventLog for polybar
-- | From: https://github.com/polybar/polybar/wiki/User-contributed-module

module Bus.LogHook
  ( logHooker
  )
where

import           Control.Monad                  ( forM_ )
import           Control.Lens

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
  concat ["%{T2}%{F", colour, "} ", logo, "%{T-}%{F-} ", rep]


countWindows :: W.StackSet i l a s sd -> Int
countWindows wins = case W.stack . W.workspace . W.current $ wins of
  Nothing -> (-1)
  Just s  -> length $ W.up s ++ W.down s


layoutParse :: String -> String
layoutParse s | s == "Three Columns" = mkLayoutStr base13 "+|+" "TCM "
              | s == "BSP"           = mkLayoutStr base13 "||+" "BSP "
              | s == "Tall"          = mkLayoutStr base13 "|||" "Tall"
              | s == "Tabbed"        = mkLayoutStr base13 "___" "Tab "
              | s == "Float"         = mkLayoutStr base13 "+++" "FLT "
              | s == "Fullscreen"    = mkLayoutStr base13 "| |" "Full"
              | otherwise            = s -- fallback for changes in C.Layout


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

  -- ^ output string based on workspaces list
  let wsStr = fmt $ W.currentTag winset

  -- ^ current layout
      ltStr =
        layoutParse . description . W.layout . W.workspace . W.current $ winset

  -- ^ strings for window operations
      controls = case countWindows winset of
        (-1) -> (copyBlank, fullBlank, killBlank)
        _    -> (copyCommand, fullCommand, killCommand)

-- pushing logs to pipes, note all files are FIFO specials
-- done this way to 'future-proof' against any stupid ideas I have
  forM_
    [ ("/tmp/xmonad-wspace", wsStr ++ "\n")
    , ("/tmp/xmonad-layout", ltStr ++ "\n")
    , ("/tmp/xmonad-copy"  , view _1 controls ++ "\n")
    , ("/tmp/xmonad-full"  , view _2 controls ++ "\n")
    , ("/tmp/xmonad-kill"  , view _3 controls ++ "\n")
    ]
    write


-- Strings ----------------------------------------------------------------------
killCommand :: String
-- killCommand = "%{A1:xdotool key Super a q:}%{F#e9436f}\xf192%{F-}%{A}"
killCommand =
  "%{A1:sleep 0.1; xdotool key Escape Super a q:}%{F#e9436f}\xf111%{F-}%{A}"

copyCommand :: String
-- copyCommand = "%{A1:xdotool key Super w c:}%{F#f09383}\xf004%{F-}%{A}"
copyCommand =
  "%{A1:sleep 0.2; xdotool key Escape Super w c:}%{F#f09383}\xf111%{F-}%{A}"

fullCommand :: String
-- fullCommand = "%{A1:xdotool key Super l 1 Super w c:}%{F09f7a0}\xf005%{F-}%{A}"
fullCommand =
  "%{A1:polybar-msg cmd toggle; xdotool key Escape Super l 1 Super w c:}%{F09f7a0}\xf111%{F-}%{A}"

-- unused because not constant width icons
-- noneCommand :: String
-- noneCommand = concat ["%{F", base11, "}\xf004%{F-}"]

-- not all of these icons are the same width so using all three again
killBlank :: String
-- killBlank = concat ["%{F", basebg, "}\xf192%{F-}"]
killBlank = concat ["%{F", basebg, "}\xf111%{F-}"]

copyBlank :: String
-- copyBlank = concat ["%{F", basebg, "}\xf004%{F-}"]
copyBlank = concat ["%{F", basebg, "}\xf111%{F-}"]

fullBlank :: String
-- fullBlank = concat ["%{F", basebg, "}\xf005%{F-}"]
fullBlank = concat ["%{F", basebg, "}\xf111%{F-}"]
