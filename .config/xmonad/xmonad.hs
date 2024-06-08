{-
  Inspired by:
  https://github.com/Axarva/dotfiles-2.0/
  https://gitlab.com/dwt1/dotfiles/.config/xmonad/
-}

import XMonad

-- Data

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)

-- Actions
import XMonad.Actions.SpawnOn (spawnOn, spawnHere)
import XMonad.Actions.MouseResize
import XMonad.Actions.GridSelect
import XMonad.Actions.Search as Se
-- Utilities 
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce (spawnOnce)

-- Layouts
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.Fullscreen
import XMonad.Layout.ThreeColumns

-- Haskell
import Control.Monad (join, when)
import Data.Maybe (maybeToList)

myTerminal :: String
myTerminal = "wezterm"

myFont :: String
myFont = "xft:Comic Mono:regular:size=12:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor   = "#c5dde0"

myFocusColor :: String
myFocusColor  = "#6eaae6"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myStartupHook :: X ()
myStartupHook = do
  spawn "xset r rate 200 60"
  spawn "xset b off"
  spawnOnce "nm-applet"
  spawnOnce "blueberry-tray"
  spawnOnce "emacs --daemon"
  spawnOnce "feh --bg-fill ~/Pictures/Wallpapers/default.png"
  spawnOnce "dunst"
  spawnOnce "flameshot"
  spawnOnce "picom --daemon"
  spawnOnce "discover-overlay --configure"
  spawnOnce "playme -t ~/.local/audio/StickerbushSymphony.mp3 -d 1"


myWorkspaces = ["1", "2", "3", "4",  "5", "6", "7", "8", "9"]

myLayouts = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- number of windows in the master pane
    ratio   = 1/2    -- proportion of screen occupied by master pane
    delta   = 3/100  -- resize incrementations 


addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ myConfig

myConfig = def
    { modMask            = mod4Mask
    , startupHook        = myStartupHook >> addEWMHFullscreen
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    , layoutHook         = smartSpacingWithEdge 4 $ myLayouts
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    }
  `additionalKeysP`
    [ -- Applications 
    ("M-<Return>", spawn myTerminal)
    , ("M-S-e", spawn "emacsclient -c")
    , ("M-S-n", spawn "wezterm -e nvim")
    , ("M-S-b", spawn "firefox-aurora")
    , ("M-p", spawnOn "5" "pavucontrol")
    , ("M-S-t", spawn "thunar")
    , ("M-S-s", spawn "./.nix-profile/bin/spotify")
    , ("M-S-d", spawn "./.nix-profile/bin/discord")

    -- Window Management
    , ("M-w", kill)
    ]
