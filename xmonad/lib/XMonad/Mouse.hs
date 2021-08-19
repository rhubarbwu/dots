{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Mouse (myMouse) where

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit (ExitCode (ExitSuccess), exitWith)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)
import qualified XMonad.StackSet as W

myMouse (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ -- Set the window to floating mode and move by dragging
      ((wMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
      -- Raise the window to the top of the stack
      ((wMask, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- Set the window to floating mode and resize by dragging
      ((wMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

wMask = mod4Mask
