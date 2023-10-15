-- Imports for XMonad --
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WorkspaceHistory

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.GridSelect as GS
import XMonad.Actions.CycleWindows as CW

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import Data.Tree
import qualified Data.Map as M
-- ............................................................................... --
myTerminal :: String
myTerminal = "kitty"

myEditor :: String
myEditor = "emacs"

myBrowser :: String
myBrowser = "firefox"

myModMask :: KeyMask
myModMask = mod4Mask

altModMask :: KeyMask
altModMask = mod1Mask

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
  spawnOnce "picom --daemon &"
  spawnOnce "feh --bg-scale ~/.emacs.d/.custom/wallpapers/nix-snowflake.png"
  windows $ W.greedyView "Dev.Snormacs"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myWorkspaces :: Forest String
myWorkspaces = [ Node "Dev"
                   [ Node "Snormacs" []
                   , Node "Terminal" []
                   , Node "Docs" []
                   , Node "Godot"
                     [ Node "Engine" []
                     , Node "Docs" []
                     , Node "Shell" []
                     ]
                   , Node "Qemu" []
                   ]
               , Node "Web"
                   [ Node "Firefox" []
                   , Node "Discord" []
                   , Node "Spotify" []
                   , Node "Thunderbird" []
                   , Node "Tor" []
                   ]
               , Node "Sys"
                   [ Node "Thunar" []
                   , Node "Pavucontrol" []
                   , Node "Gimp&Krita" []
                   ]
              , Node "Gaming"
                   [ Node "Steam-Launcher" []
                   , Node "Steam-Running" []
                   , Node "Dolphin-Emu" []
                   , Node "SNES" []
                   , Node "Lutris" []
                   , Node "Wine-Debug" []
                   ]
               , Node "Video"
                   [ Node "Obs-Studio" []
                   , Node "Demo-Win" []
                   , Node "Vlc" []
                   ]
               ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = False
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_font = "xft:Comic Mono"
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_q),        TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)

    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)

    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Master pane windows
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

    , workspaces = TS.toWorkspaces myWorkspaces
    , logHook = workspaceHistoryHook
    , layoutHook = spacingWithEdge 10 $ myLayout
    , manageHook = myManageHook
    , focusFollowsMouse = myFocusFollowsMouse

    , normalBorderColor   = myNormalBorderColor
    , focusedBorderColor  = myFocusBorderColor
    , borderWidth         = myBorderWidth
    }
    
  `additionalKeysP`
    [ ("M-<Return>", spawn (myTerminal))

    -- Mod+Space | Applications 
    , ("M-<Space> e", spawn (myEditor))
    , ("M-<Space> b", spawn (myBrowser))
    , ("M-<Space> s", spawn "spotify")
    , ("M-<Space> d", spawn "discord")
    , ("M-<Space> p", spawn "pavucontrol")

    , ("M-d", spawn "rofi -show drun")

    -- XMonad/Window Management
    , ("M-r", spawn "xmonad --recompile; xmonad --restart")
    , ("M-f", TS.treeselectWorkspace tsDefaultConfig myWorkspaces W.greedyView)
    , ("M-w", kill)

    -- Layout Maanagement

    -- FN + XF86 Keys | Multimedia Options
    , ("<XF86HomePage>", TS.treeselectWorkspace tsDefaultConfig myWorkspaces W.greedyView)
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +200")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 200-")
    , ("<SunPrint_Screen>", spawn "flameshot gui")
    , ("<XF86Explorer>", spawn "flameshot gui") 
    , ("<XF86Search>", spawn "kitty")

    -- Workspaces | Managed by TreeSelect
    , ("M-`", windows $ W.greedyView "Dev.Snormacs")
    , ("M-1", windows $ W.greedyView "Web.Firefox")
    , ("M-2", windows $ W.greedyView "Dev.Terminal")
    , ("M-3", windows $ W.greedyView "Web.Spotify")
    , ("M-4", windows $ W.greedyView "Web.Discord")
    , ("M-5", windows $ W.greedyView "Sys.Pavucontrol")
    , ("M-6", windows $ W.greedyView "Gaming.Steam-Launcher")
    , ("M-7", windows $ W.greedyView "Sys.Thunar")
    , ("M-8", windows $ W.greedyView "Web.Thunderbird")
    , ("M-9", windows $ W.greedyView "Dev.Qemu")
    , ("M-0", windows $ W.greedyView "Video.Obs-Studio")

    , ("M1-`", windows $ W.greedyView "Video.Vlc")
    , ("M1-1", windows $ W.greedyView "Dev.Godot.Engine")
    , ("M1-2", windows $ W.greedyView "Web.Tor")
    , ("M1-3", windows $ W.greedyView "Sys.Gimp&Krita")
    , ("M1-4", windows $ W.greedyView "Dev.Qemu")
    , ("M1-5", windows $ W.greedyView "Gaming.Steam-Running")
    , ("M1-6", windows $ W.greedyView "Gaming.Lutris")
    , ("M1-7", windows $ W.greedyView "Gaming.Dolphin-Emu") -- Rest of keypad for ScratchPads

    , ("M-S-`", windows $ W.shift "Dev.Snormacs")
    , ("M-S-1", windows $ W.shift "Web.Firefox")
    , ("M-S-2", windows $ W.shift "Dev.Terminal")
    , ("M-S-3", windows $ W.shift "Web.Spotify")
    , ("M-S-4", windows $ W.shift "Web.Discord")
    , ("M-S-5", windows $ W.shift "Sys.Pavucontrol")
    , ("M-S-6", windows $ W.shift "Gaming.Steam-Launcher")
    , ("M-S-7", windows $ W.shift "Sys.Thunar")
    , ("M-S-8", windows $ W.shift "Web.Thunderbird")
    , ("M-S-9", windows $ W.shift "Dev.Qemu")
    , ("M-S-0", windows $ W.shift "Video.Obs-Studio")

    , ("M1-S-`", windows $ W.shift "Video.Vlc")
    , ("M1-S-1", windows $ W.shift "Dev.Godot.Engine")
    , ("M1-S-2", windows $ W.shift "Web.Tor")
    , ("M1-S-3", windows $ W.shift "Sys.Gimp&Krita")
    , ("M1-S-4", windows $ W.shift "Dev.Qemu")
    , ("M1-S-5", windows $ W.shift "Gaming.Steam-Running")
    , ("M1-S-6", windows $ W.shift "Gaming.Lutris")
    , ("M1-S-7", windows $ W.shift "Gaming.Dolphin-Emu")
    ]
