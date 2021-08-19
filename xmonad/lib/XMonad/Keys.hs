{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Keys (extraKeys, layoutKeys) where

import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.Exit (ExitCode (ExitSuccess), exitWith)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS)
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink))
import XMonad.Layout.Spacing
  ( decScreenSpacing,
    decWindowSpacing,
    incScreenSpacing,
    incWindowSpacing,
    toggleScreenSpacingEnabled,
    toggleWindowSpacingEnabled,
  )
import qualified XMonad.StackSet as W

extraKeys :: [(String, X ())]
extraKeys = audioKeys ++ appKeys ++ musicKeys ++ displayKeys ++ windowKeys

appKeys :: [(String, X ())]
appKeys =
  [ ("M-f", spawn "nautilus"),
    ("M-S-<Return>", spawn "xterm"),
    ("M-w", spawn "brave-beta"),
    ("M-S-w", spawn "brave-beta --incognito"),
    ("M-C-w", spawn "brave-beta --incognito --tor"),
    ("M-<Space>", spawn "rofi -m -4 -show run"),
    ("M-s", spawn "sh  ~/scripts/screenshot.sh")
  ]

audioKeys :: [(String, X ())]
audioKeys =
  [ ("<XF86AudioMute>", spawn "sh ~/scripts/volume.sh mute"),
    ("<XF86AudioLowerVolume>", spawn "sh ~/scripts/volume.sh down"),
    ("<XF86AudioRaiseVolume>", spawn "sh ~/scripts/volume.sh up"),
    ("<XF86AudioMicMute>", spawn "sh ~/scripts/volume.sh mic"),
    ("M-a", spawn "sh ~/scripts/sinks.sh"),
    ("M-S-a", spawn "pavucontrol")
  ]

musicKeys :: [(String, X ())]
musicKeys =
  [ ("<XF86AudioPlay>", spawn "mpc toggle"),
    ("<XF86AudioPrev>", spawn "mpc prev"),
    ("<XF86AudioNext>", spawn "mpc next"),
    ("M-\\", spawn "mpc toggle"),
    ("M-[", spawn "mpc prev"),
    ("M-]", spawn "mpc next"),
    ("M-S-m", spawn "xterm -e 'ncmpcpp'"),
    ("M-S-v", spawn "xterm -e 'vis'")
  ]

displayKeys :: [(String, X ())]
displayKeys =
  [ ("M-d", spawn "sh ~/scripts/displays.sh -t"),
    ("M-S-d", spawn "pkill arandr && arandr"),
    ("M-C-d", spawn "autorandr --load mobile"),
    ("<XF86MonBrightnessDown>", spawn "sh ~/scripts/brightness.sh down"),
    ("<XF86MonBrightnessUp>", spawn "sh ~/scripts/brightness.sh up")
  ]

windowKeys :: [(String, X ())]
windowKeys =
  [ -- Workspaces
    ("M-l", nextWS),
    ("M-h", prevWS),
    ("M-S-l", shiftToNext >> nextWS),
    ("M-S-h", shiftToPrev >> prevWS),
    ("M-i", toggleWS),
    --

    -- Window/Screen spacing
    ("M--", incScreenSpacing 3),
    ("M-=", decScreenSpacing 3),
    ("M-S--", incWindowSpacing 3),
    ("M-S-=", decWindowSpacing 3),
    --

    -- Focus/swap next, prev, master windows.
    ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-m", windows W.focusMaster),
    ("M-<Return>", windows W.swapMaster),
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp),
    --

    -- Kill one; kill all
    ("M-q", kill1),
    ("M-S-q", killAll),
    -- Sink window into tiling
    ("M-t", withFocused $ windows . W.sink),
    -- Shrink/expand the master/slave area
    ("M-<Left>", sendMessage Shrink),
    ("M-<Right>", sendMessage Expand),
    ("M-<Down>", sendMessage MirrorShrink),
    ("M-<Up>", sendMessage MirrorExpand),
    -- Increment/decrement the number of windows in the master area
    ("M-,", sendMessage (IncMasterN 1)),
    ("M-.", sendMessage (IncMasterN (-1))),
    --

    -- Bar; gaps
    ("M-b", sendMessage ToggleStruts),
    ("M-g", toggleScreenSpacingEnabled <+> toggleWindowSpacingEnabled),
    --

    -- Lock; exit; restart; colors
    ("M-<Escape>", spawn "betterlockscreen -l"),
    ("M-S-<Escape>", spawn "sh ~/scripts/widgets.sh" <+> io (exitWith ExitSuccess)),
    ("M-r", spawn "sh ~/scripts/x11.sh"),
    ("M-S-r", spawn "sh ~/scripts/xmonad.sh"),
    ("M-c", spawn "sh ~/scripts/xcolor.sh"),
    --

    -- Help
    ("M-S-/", spawn ("printf " ++ show help ++ " | xmessage -file -"))
  ]

layoutKeys conf@(XConfig {XMonad.modMask = wMask}) =
  M.fromList $
    []
      -- Layouts
      ++ [ ((wMask, xK_Tab), sendMessage NextLayout),
           ((wMask .|. sMask, xK_Tab), setLayout $ XMonad.layoutHook conf)
         ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. wMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, sMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. cMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, sMask)]
      ]

help = unlines []

aMask = mod1Mask

cMask = controlMask

sMask = shiftMask

wMask = mod4Mask
