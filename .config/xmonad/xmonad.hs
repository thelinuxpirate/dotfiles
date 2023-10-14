import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.EwmhDesktops

myTerminal :: String
myTerminal = "kitty"

myBrowser :: String
myBrowser = "firefox"

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myNormalBorderColor :: String
myNormalBorderColor = "#6655a3"

myFocusBorderColor :: String
myFocusBorderColor = "#583ac2"

myBorderWidth :: Dimension
myBorderWidth = 2
-- ............................................................................... --
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xset r rate 200 60"
  spawnOnce "xset b off"
  spawnOnce "feh --bg-scale ~/.emacs.d/.custom/wallpapers/nix-snowflake.png"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     $ myConfig

myConfig = def
    { startupHook = myStartupHook
    , modMask    = mod4Mask
    , layoutHook = spacingWithEdge 10 $ myLayout
    , manageHook = myManageHook
    , focusFollowsMouse = myFocusFollowsMouse

    , normalBorderColor   = myNormalBorderColor
    , focusedBorderColor  = myFocusBorderColor
    , borderWidth         = myBorderWidth
    }
    
  `additionalKeysP`
    [ ("M-<Return>", spawn (myTerminal))
    , ("M-S-d", spawn "discord")
    , ("M-S-b"  , spawn (myBrowser))
    , ("M-w", kill)
    ]
