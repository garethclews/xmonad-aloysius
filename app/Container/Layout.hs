{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- XMonad-Aloysius xmonad layouts

module Container.Layout where


-- Imports ----------------------------------------------------------------------
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Foreign.C.Types                ( CInt )

import           XMonad                  hiding ( (|||) )

import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.MouseResize     ( MouseResize(..) )

import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutCombinators
                                                ( JumpToLayout(..)
                                                , (|||)
                                                )
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed          ( Rename(..) )
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Simplest         ( Simplest(..) )
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowArranger   ( WindowArranger )

import           XMonad.StackSet               as W
                                         hiding ( focus )

import           Config.Options
import           Config.Projects
import           Container.IfMax
-- my version of X.L.IfMax without the annoying bug and a little code
-- refactoring to appease the hlint gods


------------------------------------------------------------------------
--
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- Types ------------------------------------------------------------------------
data Gaps' = Gaps'
  { u  :: Int
  , d  :: Int
  , x  :: Int
  , x' :: Integer
  }


newtype ADecoration a = AD Bool deriving (Read, Show)

newtype SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

-- type to wrap the actual layout within all the mess I have created
type ALayoutIfFull a
  = ModifiedLayout
      Rename
      ( IfMax
          ( ModifiedLayout
              Rename
              (ModifiedLayout WithBorder (ModifiedLayout FullscreenFull Full))
          )
          ( ModifiedLayout
              Gaps
              ( ModifiedLayout
                  (Decoration ADecoration DefaultShrinker)
                  (ModifiedLayout DraggingVisualizer (ModifiedLayout Spacing a))
              )
          )
      )
      Window

type ADecorated = Decoration ADecoration DefaultShrinker


-- Instances --------------------------------------------------------------------

-- | side decorations
instance Eq a => DecorationStyle SideDecoration a where
  describeDeco _ = "Side handles"
  shrink b (Rectangle _ _ dw dh) (Rectangle _x _y w h)
    | SideDecoration U <- b = Rectangle _x (_y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle _x _y (w - dw) h
    | SideDecoration D <- b = Rectangle _x _y w (h - dh)
    | SideDecoration L <- b = Rectangle (_x + fi dw) _y (w - dw) h
  pureDecoration b dw dh _ st _ (win, Rectangle _x _y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle _x _y w dh
      SideDecoration R -> Rectangle (_x + fi (w - dw)) _y dw h
      SideDecoration D -> Rectangle _x (_y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle _x _y dw h
    | otherwise = Nothing


-- | clickable bar default decoration :)
instance Eq a => DecorationStyle ADecoration a where
  describeDeco _ = "AloyDecoration"
  decorationCatchClicksHook _ mainw dFL dFR = clickHandler mainw dFL dFR
  decorationWhileDraggingHook _ ex ey (mainw, r) xx yy =
    handleTiledDraggingInProgress ex ey (mainw, r) xx yy
  decorationAfterDraggingHook _ (mainw, _) decoWin = do
    focus mainw
    hasCrossed <- handleScreenCrossing mainw decoWin
    unless hasCrossed $ do
      sendMessage DraggingStopped
      performWindowSwitching mainw


-- Functions --------------------------------------------------------------------

gs :: Gaps'
gs = Gaps' { u = 20, d = 20, x = 20, x' = 20 }


gapses :: l a -> ModifiedLayout Gaps l a
gapses = gaps [(U, u gs), (R, x gs), (L, x gs), (D, d gs)]

spacingses :: l a -> ModifiedLayout Spacing l a
spacingses = spacingRaw True
                        (Border 0 (x' gs) (x' gs) (x' gs))
                        True
                        (Border (x' gs) (x' gs) (x' gs) (x' gs))
                        True

-- my decoration Aloy DECOrations
sidedeco
  :: Eq a
  => l a
  -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
sidedeco = decoration shrinkText sideDecoTheme (SideDecoration L)

-- customised layouts
-- these type signatures though...
full
  :: ModifiedLayout
       Rename
       (ModifiedLayout WithBorder (ModifiedLayout FullscreenFull Full))
       Window
full = named "Fullscreen" $ noBorders (fullscreenFull Full)


tall :: ALayoutIfFull ResizableTall
tall =
  named "Tall"
    . IfMax 1 full
    . gapses
    . aDecoration shrinkText decoTheme
    . draggingVisualizer
    . spacingses
    $ ResizableTall 1 (2 / 100) (1 / 2) []


tcm :: ALayoutIfFull ThreeCol
tcm =
  named "Three Columns"
    . IfMax 1 full
    . gapses
    . aDecoration shrinkText decoTheme
    . draggingVisualizer
    . spacingses
    $ ThreeColMid 1 (1 / 10) (1 / 2)

tabs
  :: ModifiedLayout
       Rename
       (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)
       Window
tabs = named "Tabbed" $ tabbedBottom shrinkText tabTheme


flt
  :: ModifiedLayout
       Rename
       ( ModifiedLayout
           (Decoration SideDecoration DefaultShrinker)
           ( ModifiedLayout
               (Decoration SimpleDecoration DefaultShrinker)
               ( ModifiedLayout
                   MouseResize
                   (ModifiedLayout WindowArranger SimpleFloat)
               )
           )
       )
       Window
flt = named "Float" . sidedeco $ simpleFloat' shrinkText emptyTheme


-- layout --
layout =
  avoidStruts
    .   smartBorders
    .   onWorkspace wsScratch flt
    $   tall
    ||| tabs
    ||| full


-- Instance support -------------------------------------------------------------
buttonOffset :: Int
buttonOffset = 24

buttonSize :: Int
buttonSize = 24 :: Int


-- button location constraints
-- TODO: abstract this out
-- | right button
rLE :: Int
rRE :: Int
rLE = buttonOffset + buttonSize
rRE = buttonOffset

-- | middle button
mLE :: Int
mRE :: Int
mLE = buttonOffset + 2 * buttonSize
mRE = rLE

-- left button
lLE :: Int
lRE :: Int
lLE = buttonOffset + 3 * buttonSize
lRE = mLE


-- click handler
clickHandler :: Window -> Int -> Int -> X Bool
clickHandler mainw _ dR = do
  let action
        | dR >= rRE && dR <= rLE
        = focus mainw >> kill >> return True
        | dR >= mRE && dR <= mLE
        = sendMessage (JumpToLayout "Fullscreen")
          >> sendMessage ToggleStruts
          >> spawn "polybar-msg cmd toggle"
          >> return True
        | dR >= lRE && dR <= lLE
        = focus mainw >> tagToEmptyWorkspace >> return True
        | otherwise
        = return False
  action


-- | dragging windows
handleTiledDraggingInProgress
  :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleTiledDraggingInProgress ex ey (mainw, r) xx yy = do
  let rect = Rectangle (xx - (fi ex - rect_x r))
                       (yy - (fi ey - rect_y r))
                       (rect_width r)
                       (rect_height r)
  sendMessage $ DraggingWindow mainw rect


-- switching window positions by dragging
performWindowSwitching :: Window -> X ()
performWindowSwitching win = withDisplay $ \dd -> do
  root                          <- asks theRoot
  (_, _, selWin, _, _, _, _, _) <- io $ queryPointer dd root
  ws                            <- gets windowset
  let allWindows' = W.index ws
  -- do a little double check to be sure
  when ((win `elem` allWindows') && (selWin `elem` allWindows')) $ do
    let allWindowsSwitched = map (switchEntries win selWin) allWindows'
        (ls, t : rs)       = break (win ==) allWindowsSwitched
        newStack           = W.Stack t (reverse ls) rs
    windows $ W.modify' $ const newStack
 where
  switchEntries a b xx | xx == a   = b
                       | xx == b   = a
                       | otherwise = xx


-- decoration enabler
aDecoration
  :: (Eq a, Shrinker s)
  => s
  -> Theme
  -> l a
  -> ModifiedLayout (Decoration ADecoration s) l a
aDecoration s c = decoration s c $ AD False
