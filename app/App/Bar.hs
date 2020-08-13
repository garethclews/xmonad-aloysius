{-# LANGUAGE OverloadedStrings #-}
-- WIP: potential abandonware
-- | Possibly some attempt at a dashboard type deal

module App.Bar where

import           Control.Concurrent             ( threadDelay )

import           XMonad
import           XMonad.Util.Font               ( initXMF
                                                , Align(AlignCenter)
                                                )
import           XMonad.Util.XUtils

bar :: X ()
bar = do
  let mask = Just exposureMask -- .|. buttonPressMask

  -- windows
  header <- createNewWindow (Rectangle 100 400 300 400) mask "#1c1e26" False

  let ws = [header]

  -- settings

  f <- initXMF "xft:mplus Nerd Font:style=Medium:size=12"
  paintAndWrite header
                f
                300
                400
                2
                "#1c1e26"
                "#16161c"
                "#ffffff"
                "#1c1e26"
                [AlignCenter]
                ["yep"]
  showWindow header

  liftIO $ threadDelay (5 * 1000000)
  deleteWindow header
