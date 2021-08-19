{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout (myLayout) where

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Spacing (Border (..), spacing, spacingRaw)

myLayout =
  buttonDeco shrinkText defaultThemeWithButtons $
    avoidStruts $
      spacingRaw False (Border 36 36 36 36) True (Border 12 12 12 12) True $
        (zen ||| dwm)
  where
    -- default tiling algorithm partitions the screen into two panes
    dwm = smartBorders $ ResizableTall nmaster delta ratio []
    nmaster = 1 -- Default number of windows in the master pane
    ratio = (3 / 5) -- Default proportion of screen occupied by master pane
    delta = (1 / 50) -- Percent of screen to increment by when resizing panes

    -- full-screen zen mode
    zen = noBorders Full