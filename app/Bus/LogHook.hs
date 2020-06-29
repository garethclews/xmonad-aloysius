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
  let currWs = W.currentTag winset

  -- ^ output string based on workspaces list
  let wsStr  = fmt currWs

  -- ^ focussed window
  -- fcStr <- maybe (return "") (fmap show . getName) . W.peek $ winset

  -- ^ current layout
  let ltStr =
        layoutParse . description . W.layout . W.workspace . W.current $ winset


  let snStr = case countWindows winset of
        0 -> "\xf004"  -- 
        _ -> concat ["%{F", base03, "}\xf004%{F-}"]

  -- pushing logs to pipes, note all files are FIFO specials
  -- done this way to 'future-proof' against any stupid ideas I have
  forM_
    [ ("/tmp/xmonad-wspace", wsStr ++ "\n")
    , ("/tmp/xmonad-layout", ltStr ++ "\n")
    , ("/tmp/xmonad-curwin", snStr ++ "\n")
    ]
    write


countWindows :: W.StackSet i l a s sd -> Int
countWindows wins = case W.stack . W.workspace . W.current $ wins of
  Nothing -> 1
  Just s  -> length $ W.up s ++ W.down s
