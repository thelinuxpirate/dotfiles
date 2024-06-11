{-
  Inspired by:
  https://gitlab.com/dwt1/dotfiles/.config/xmonad/
  https://github.com/GlitchMill/dotfiles/tree/Fuji
  https://github.com/Axarva/dotfiles-2.0/
-}

-- Base
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Prompt as P
import Theme.Trong

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageHelpers (doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.ManageDocks

-- Actions
import XMonad.Actions.ToggleFullFloat
import XMonad.Actions.Submap (submap)
import XMonad.Actions.DwmPromote
import XMonad.Actions.MouseResize
import XMonad.Actions.Minimize
import XMonad.Actions.CycleWindows
import XMonad.Actions.GridSelect
import XMonad.Actions.Search as Se

-- Utilities 
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run

-- Layouts -- ADD grid; mirror tall;
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull)
import XMonad.Layout.CenterMainFluid
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as Bw (boringWindows, siftUp, siftDown)

-- Haskell
import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.List (isInfixOf, intercalate)
import System.Process (readProcessWithExitCode, callProcess)
import System.Exit (ExitCode(..))

myHome :: String
myHome = "/home/trong/"

myTerminal :: String
myTerminal = "wezterm"

myFont :: String
myFont = "xft:Comic Mono:regular:size=12:antialias=true:hinting=true"

myStartupHook :: X ()
myStartupHook = do
  spawn "xset r rate 200 60"
  spawn "xset b off"
  spawnOnce "nm-applet"
  spawnOnce "blueman-applet"
  spawnOnce "emacs --daemon"
  spawnOnce "dynamic-paper.c"
--  spawnOnce "feh --bg-fill ~/Pictures/Wallpapers/etc/bender.jpg"
  spawnOnce "polybar -c ~/.config/xmonad/polybar/config.ini"
  spawnOnce "dunst"
  spawnOnce "flameshot"
  spawnOnce "picom --daemon"
  spawnOnce "greenclip daemon"
  spawnOnce "discover-overlay --configure"
  spawnOnce "playme -t ~/.local/audio/StickerbushSymphony.mp3 -d 1"

myLogHook :: X ()
myLogHook = writeLayoutToFile

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

myLayouts = avoidStruts $ Bw.boringWindows
  (tiled |||
  Mirror tiled |||
  CenterMainFluid 1 (3/100) (70/100) |||
  Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 3/5
    delta   = 3/100 

myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
  [ insertPosition End Newer
  , className    =? "discover-overlay"    --> doShift "tmp"
  , className    =? "Pavucontrol"         --> doShift "5"
  , className    =? "Gimp"                --> doFloat
  , isDialog                              --> doF W.swapUp
  , resource     =? "desktop_window"      --> doIgnore
  , isFullscreen                          --> doFullFloat
  ]

mySearchList :: [(String, Se.SearchEngine)]
mySearchList = [ ("g",     Se.github)
               , ("S-g",   Se.google)
               , ("y",     Se.youtube)
               , ("c",     Se.cratesIo)
               , ("h",     Se.homeManager)
               , ("S-h",   Se.hoogle)
               , ("s",     Se.steam)
               , ("S-s",   Se.protondb)
               , ("n",     Se.nixos)
               , ("w",     Se.wikipedia)
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

getCurrentLayout :: X String
getCurrentLayout = withWindowSet $ \ws -> do
    let layout = description . W.layout . W.workspace . W.current $ ws
    return layout

cleanLayout :: String -> String
cleanLayout layout = intercalate " " $ filter (\s -> not $ any (`isInfixOf` s) ["Spacing", "Minimize"]) (words layout)

toggleFullscreen :: X ()
toggleFullscreen = do
    winset <- gets windowset
    layout <- getCurrentLayout
    let currLayout = cleanLayout layout
    if currLayout == "Full"
    then sendMessage $ JumpToLayout "Tall"
    else sendMessage $ JumpToLayout "Full"

writeLayoutToFile :: X ()
writeLayoutToFile = do
    layout <- getCurrentLayout
    let cleanLayoutStr = cleanLayout layout
    let filePath = myHome ++ ".config/xmonad/lib/.layout.txt"
    io $ writeFile filePath cleanLayoutStr

toggleFloat :: X ()
toggleFloat = withFocused $ \win -> do
    winset <- gets windowset
    let isFloating = M.member win (W.floating winset)
    if isFloating
    then withFocused $ windows . W.sink
    else withFocused $ windows . (flip W.float $ W.RationalRect 0.1 0.1 0.8 0.8)

-- XMessage Demo Function
-- showCurrentLayout :: X ()
-- showCurrentLayout = do
--     layout <- getCurrentLayout
--     safeSpawn "xmessage" [layout]

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
     . toggleFullFloatEwmhFullscreen
     . ewmhFullscreen 
     . ewmh 
     $ myConfig

myConfig = def
    { modMask            = mod4Mask
    , startupHook        = myStartupHook >> addEWMHFullscreen
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    , layoutHook         = smartSpacingWithEdge 4 $ minimize . Bw.boringWindows $ myLayouts
    , logHook            = myLogHook
    , manageHook         = myManageHook
    , borderWidth        = 2
    , focusedBorderColor = colorFocused
    , normalBorderColor  = colorUnfocused
    , focusFollowsMouse  = False
    , clickJustFocuses   = True
    }
  `additionalKeysP`
    [ -- Applications 
    ("M-<Return>",    spawn myTerminal)
    , ("M-<Space>",   spawn "rofi -show-icons -show drun")
    , ("M-C-c",       spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command \'{cmd}\'")
    , ("M-S-e",       spawn "emacsclient -c")
    , ("M-S-n",       spawn "wezterm -e nvim")
    , ("M-S-b",       spawn "firefox")
    , ("M-p",         spawn "pavucontrol")
    , ("M-S-t",       spawn "thunar")
    , ("M-S-s",       spawn "flatpak run com.spotify.Client")
    , ("M-S-d",       spawn "discord")

    -- Window Management
    , ("M-S-r",                  spawn "xmonad --restart")
    , ("M-w",                    kill)
    , ("M-S-u",                  writeLayoutToFile)
    , ("M-b",                    togglePolybar)
    , ("M-C-k",                  Bw.siftUp)
    , ("M-C-j",                  Bw.siftDown)
    , ("M-i",                    windows $ W.focusMaster)
    , ("M-S-<Return>",           dwmpromote)
    , ("M-u",                    sendMessage $ NextLayout)
    , ("M-f",                    toggleFullscreen)
    , ("M-S-f",                  withFocused toggleFullFloat)
    , ("M-S-<Space>",            toggleFloat) 
    , ("M-m",                    withFocused minimizeWindow)
    , ("M-S-m",                  withLastMinimized maximizeWindowAndFocus)

    -- Layouts Submap
    , ("M-S-l", submap . M.fromList $
       [ ((0, xK_t),     sendMessage $ JumpToLayout "Tall")
       , ((0, xK_f),     sendMessage $ JumpToLayout "Full")
       , ((0, xK_g),     sendMessage $ JumpToLayout "Grid")
       ])
    
    -- XMonad Extras


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
    ++ [("M-s " ++ k, Se.promptSearch P.def f) | (k,f) <- mySearchList]
    ++ [("M-S-s " ++ k, Se.selectSearch f) | (k,f) <- mySearchList ]
    `additionalKeys` (extraWorkspaces)
