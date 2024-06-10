{-
  Inspired by:
  https://gitlab.com/dwt1/dotfiles/.config/xmonad/
  https://github.com/GlitchMill/dotfiles/tree/Fuji
  https://github.com/Axarva/dotfiles-2.0/
-}

-- Base
import XMonad
import qualified XMonad.StackSet as W
import Theme.Trong as T

-- Data
import qualified Data.Map as M
import Data.Maybe (maybeToList)

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageHelpers (doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.ManageDocks

-- Actions
import XMonad.Actions.SpawnOn (spawnOn, spawnHere)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.MouseResize
import XMonad.Actions.Minimize
import XMonad.Actions.CycleWindows
import XMonad.Actions.GridSelect
import XMonad.Actions.Search as Se

-- Utilities 
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Layouts -- ADD grid; mirror tall;
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull)
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

-- Haskell
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import System.Process (readProcessWithExitCode, callProcess)
import System.Exit (ExitCode(..))

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
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = True

myStartupHook :: X ()
myStartupHook = do
  spawn "xset r rate 200 60"
  spawn "xset b off"
  spawnOnce "nm-applet"
  spawnOn "9" "blueman-applet"
  spawnOnce "emacs --daemon"
  spawnOnce "feh --bg-fill ~/Pictures/Wallpapers/etc/bender.jpg"
  spawnOnce "polybar -c ~/.config/xmonad/polybar/config.ini"
  spawnOnce "dunst"
  spawnOnce "flameshot"
  spawnOnce "picom --daemon"
  spawnOnce "greenclip daemon"
  spawnOnce "discover-overlay --configure"
  spawnOnce "playme -t ~/.local/audio/StickerbushSymphony.mp3 -d 1"

myExtraWorkspaces = [(xK_0, "0"),(xK_minus, "gd"),(xK_equal, "tmp")]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] ++ (map snd myExtraWorkspaces)

extraWorkspaces =
  [
    ((mod4Mask, key), (windows $ W.greedyView ws))
    | (key,ws) <- myExtraWorkspaces
  ] ++ [
    ((mod4Mask .|. shiftMask, key), (windows $ W.shift ws))
    | (key,ws) <- myExtraWorkspaces
  ]

myLayouts = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- number of windows in the master pane
    ratio   = 2/3    -- proportion of screen occupied by master pane
    delta   = 3/100  -- resize incrementations 

myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
  [ insertPosition End Newer
  , className    =? "Gimp"           --> doFloat
  , isDialog                         --> doF W.swapUp
  , resource     =? "desktop_window" --> doIgnore
  , isFullscreen                     --> doFullFloat
  ]

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

togglePolybar :: X ()
togglePolybar = do
    polybarRunning <- liftIO isPolybarRunning
    if polybarRunning
       then spawn "killall -q polybar"
       else spawnOnce "polybar -c ~/.config/xmonad/polybar/config.ini"

isPolybarRunning :: IO Bool
isPolybarRunning = do
    (exitCode, result, _) <- readProcessWithExitCode "pgrep" ["-x", "polybar"] ""
    case exitCode of
        ExitSuccess -> do
            let running = not $ null result
            if running
               then do
                   let pid = head $ lines result
                   callProcess "kill" [pid]
               else return ()
            return running
        _ -> return False   

main :: IO ()
main = xmonad 
     . docks
     . ewmhFullscreen 
     . ewmh 
     $ myConfig

myConfig = def
    { modMask            = mod4Mask
    , startupHook        = myStartupHook >> addEWMHFullscreen
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    , layoutHook         = smartSpacingWithEdge 4 $ minimize . BW.boringWindows $ myLayouts
    , manageHook         = myManageHook
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    }
  `additionalKeysP`
    [ -- Applications 
    ("M-<Return>",    spawn myTerminal)
    , ("M-<Space>",   spawn "rofi -show drun")
    , ("M-C-c",       spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command \'{cmd}\'")
    , ("M-S-e",       spawn "emacsclient -c")
    , ("M-S-n",       spawn "wezterm -e nvim")
    , ("M-S-b",       spawn "firefox")
    , ("M-p",         spawnOn "5" $ "pavucontrol")
    , ("M-S-t",       spawn "thunar")
    , ("M-S-s",       spawn "flatpak run com.spotify.Client")
    , ("M-S-d",       spawn "discord")

    -- Window Management
    , ("M-S-r",                  spawn "xmonad --restart")
    , ("M-w",                    kill)
    , ("M-b",                    togglePolybar)
    , ("M-g",                    windows W.focusMaster)
    , ("M-S-<Return>",           windows W.swapMaster)
    , ("M-S-<Space>",            withFocused $ windows . W.sink)
    , ("M-m",                    withFocused minimizeWindow)
    , ("M-S-m",                  withLastMinimized maximizeWindowAndFocus)

    -- Layouts Submap
    , ("M-S-l", submap . M.fromList $
       [ ((0, xK_t),     sendMessage $ JumpToLayout "Tall")
       , ((0, xK_f),     sendMessage $ JumpToLayout "Full")
       , ((0, xK_g),     sendMessage $ JumpToLayout "Grid")
       --, ((0, xK_space), spawn "mpc toggle")
       ])
    
    -- Media Keys
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+" )
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-" )
    , ("<XF86AudioMute>",        spawn "amixer set Master toggle" )
    , ("<XF86AudioPlay>",        spawn "playerctl play-pause" )
    , ("<XF86AudioNext>",        spawn "playerctl next" )
    , ("<XF86AudioPrev>",        spawn "playerctl previous" )
    , ("<XF86Explorer>",         spawn "flameshot gui" )
    , ("<XF86Search>",           spawn "rofi -show drun" )
    -- FN Keys (Keyboard Dependant; feel free to modify)
    ]
    `additionalKeys` (extraWorkspaces)
