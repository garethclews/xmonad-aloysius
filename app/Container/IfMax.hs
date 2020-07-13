{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, PatternGuards #-}
-- for documentation see X.L.IfMax
module Container.IfMax
  ( IfMax(..)
  , ifMax
  )
where

import           Control.Arrow
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe

import           XMonad
import qualified XMonad.StackSet               as W


data IfMax l1 l2 w = IfMax Int (l1 w) (l2 w)
  deriving (Read, Show)

instance (LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (IfMax l1 l2) Window where
  runLayout (W.Workspace wname (IfMax n l1 l2) s) rect = withWindowSet
    $ \ws -> arrange (W.integrate' s) (M.keys . W.floating $ ws)
   where
    arrange ws fw
      | length (ws L.\\ fw) <= n = do
        (wrs, ml1') <- runLayout (W.Workspace wname l1 s) rect
        let l1' = fromMaybe l1 ml1'
        l2' <- fromMaybe l2 <$> handleMessage l2 (SomeMessage Hide)
        return (wrs, Just $ IfMax n l1' l2')
      | otherwise = do
        (wrs, ml2') <- runLayout (W.Workspace wname l2 s) rect
        l1'         <- fromMaybe l1 <$> handleMessage l1 (SomeMessage Hide)
        let l2' = fromMaybe l2 ml2'
        return (wrs, Just $ IfMax n l1' l2')

  handleMessage (IfMax n l1 l2) m | Just ReleaseResources <- fromMessage m = do
    l1' <- handleMessage l1 (SomeMessage ReleaseResources)
    l2' <- handleMessage l2 (SomeMessage ReleaseResources)
    if isNothing l1' && isNothing l2'
      then return Nothing
      else return $ Just $ IfMax n (fromMaybe l1 l1') (fromMaybe l2 l2')
  handleMessage (IfMax n l1 l2) m | Just Hide <- fromMessage m = do
    l1' <- handleMessage l1 (SomeMessage ReleaseResources)
    l2' <- handleMessage l2 (SomeMessage ReleaseResources)
    if isNothing l1' && isNothing l2'
      then return Nothing
      else return $ Just $ IfMax n (fromMaybe l1 l1') (fromMaybe l2 l2')
  handleMessage (IfMax n l1 l2) m = do
    (allWindows, floatingWindows) <- gets
      ((W.integrate' . W.stack . W.workspace . W.current &&& M.keys . W.floating
       )
      . windowset
      )
    if length (allWindows L.\\ floatingWindows) <= n
      then do
        l1' <- handleMessage l1 m
        return $ flip (IfMax n) l2 <$> l1'
      else do
        l2' <- handleMessage l2 m
        return $ IfMax n l1 <$> l2'

  description (IfMax n l1 l2) =
    "If number of windows is <= "
      ++ show n
      ++ ", then "
      ++ description l1
      ++ ", else "
      ++ description l2

-- | Layout itself
ifMax
  :: (LayoutClass l1 w, LayoutClass l2 w)
  => Int            -- ^ Maximum number of windows for the first layout
  -> l1 w           -- ^ First layout
  -> l2 w           -- ^ Second layout
  -> IfMax l1 l2 w
ifMax = IfMax
