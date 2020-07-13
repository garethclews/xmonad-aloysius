{-# LANGUAGE OverloadedStrings #-}
-- WIP: potential abandonware
-- | Possibly some attempt at a dashboard type deal

module App.Bar where

import           XMonad
import           XMonad.Util.Font               ( initXMF
                                                , Align(AlignCenter)
                                                )
import           XMonad.Util.XUtils

bar :: X ()
bar = do
  let mask = Just $ exposureMask .|. buttonPressMask

  -- windows
  header <- createNewWindow (Rectangle 150 200 300 400) mask "#1c1e26" False

  let ws = [header]

  -- settings
  f <- initXMF "xft:mplus Nerd Font:style=Medium:size=12"

  showWindows ws
  paintAndWrite header
                f
                310
                52
                0
                "#1c1e26"
                "#fff"
                "#f81"
                "#1c1e26"
                [AlignCenter]
                ["yep"]
