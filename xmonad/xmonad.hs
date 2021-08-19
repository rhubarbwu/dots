{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import System.IO (hClose)
import XMonad
import XMonad.Actions.Commands
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Floating (myCentreFloating)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP (xmobarPP)
import XMonad.Keys (extraKeys, layoutKeys)
import XMonad.Layout (myLayout)
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Mouse (myMouse)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Xresources (fromXres)

myID = "22"

myManageHook =
  composeAll $
    []
      ++ map (\x -> (className =? x --> doCenterFloat)) myCentreFloating

mySB = statusBarProp "xmobar" (pure xmobarPP)

main = do
  xmonad $ withEasySB mySB defToggleStrutsKey $ config
  where
    config =
      desktopConfig
        { --
          normalBorderColor = fromXres "*.color8",
          focusedBorderColor = fromXres "*.color7",
          borderWidth = 2,
          --
          layoutHook = myLayout,
          manageHook = myManageHook,
          startupHook = do
            n <- countScreens
            mapM (\i -> spawnPipe $ "xmobar ~/.xmonad/xmobarrc -x " ++ show i) [0 .. n -1]
            spawn "pkill yeet",
          --
          modMask = mod4Mask,
          keys = layoutKeys
        }
        `additionalKeysP` extraKeys
