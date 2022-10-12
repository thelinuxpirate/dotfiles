{-                  MAP LEGEND
    The Map Legend, when searched for the specific code will take you to that location (just like a Map Legend!);
REMEMBER: The codes will begin with CAPITAL P's not lower-case like shown here! (This goes for ALL characters!);
p-1 = Imports and Modules;
p-2 = Custom Functions & Extra Code that is NOT a Part of the Main Function;
p-end = Main Function;                                                                                         -}

-- P-1, Importing modules needed to customize Xmonad:

-- Important Modules, (IO, and Default Modules):
import XMonad -- Xmonad Module
import Data.Monoid
import Data.Tree
import System.Exit
import qualified Data.Map        as M
import XMonad.Hooks.ManageDocks

-- Utilities:
import XMonad.Util.EZConfig -- Helps set keybinds for Xmonad, contains: spawn, and additionalKeys(s) functions; 
import XMonad.Util.Ungrab   -- I actually don't know/forgot;
import XMonad.Util.SpawnOnce -- Includes the "spawnOnce" function;
import XMonad.Util.SessionStart -- Includes the "doOnce" function;
import XMonad.Util.Run
import XMonad.Util.Loggers

-- Hooks:
import XMonad.Hooks.EwmhDesktops -- Imports the ability to become EWMH;
import XMonad.Hooks.DynamicLog
import XMonad.ManageHook -- Imports more Options to Customize Hooks;
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Util.ClickableWorkspaces 
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ServerMode

-- Layouts & Windows:
import XMonad.Layout -- Features to Customize Layouts;
import XMonad.Layout.Spiral -- Spiral Layout;
import XMonad.Layout.Spacing -- Adds Spacing Between Windows;
import XMonad.Layout.Gaps -- Adds Spacing Between the Edges of the Screen;
import XMonad.Layout.ThreeColumns -- Adds the Option to Use the ThreeColumns Layout;
import XMonad.Layout.Magnifier -- Adds the Option to Magnify Windows;
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Renamed -- Adds the Option to Rename Layouts;
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named

-- Keybinds & Actions:
import Graphics.X11.ExtraTypes.XF86 -- FN Audio Keys Support;
import XMonad.Actions.CopyWindow 
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import  qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.StackSet as W

-- P-2, Custom Functions & Extra: (Most Function Titles Start With "mi" at the Start of each Function); --

miStartupHook :: X () -- Creates a StartupHook for Xmonad which Autostarts Commands & Applications;
miStartupHook = do 
    spawnOnce "nitrogen --restore &" -- Autostarts Nitrogen for the Wallpaper (You Will Have to Set your own Wallpaper using Nitrogen!);
    spawnOnce "picom --daemon" -- Autostarts Picom for Transparency (YOU MUST CONFIGURE YOUR OWN PICOM.CONF);

miManageHook = composeAll
     {- Using 'doShift ( miWorkspaces !! 7)' sends Program to Workspace 8!
        I'm Doing it this way because Otherwise I Would Have to Write Out
        the Full Name of my Clickable Workspaces, Which Would Look Like:
        doShift "<action xdotool super+8>gfx</action>"     <-- Example Code; -}
     [ className =? "obs"     --> doShift ( "video.obs" )
     , title =? "firefox"     --> doShift ( "web.browser" )
     , title =? "qutebrowser" --> doShift ( "web.browser" )
     , className =? "mpv"     --> doShift ( "video.movie player" )
     , className =? "vlc"     --> doShift ( "video.movie player" )
     , className =? "Gimp"    --> doShift ( "graphics.gimp")
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "VirtualBox Manager" --> doShift  ( "dev.virtualization" )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]

miLogHook :: X ()
miLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

miTerminal :: String
miTerminal = "alacritty" -- Terminal Name Goes Here and it Will be Reapplied to the Whole Config;

miModMask :: KeyMask
miModMask = mod4Mask -- Binds the "Mod-Key" to be set as the Super-Key/Windows-Key;

miFocusFollowsMouse :: Bool
miFocusFollowsMouse = False -- If the Focus of Windows Follow the Mouse, (True or False Values);

miNormalBorderColor :: String
miNormalBorderColor = "#6655a3" -- Enter a Color Code for the Window's "Normal-Sate", Default is: "#6655a3";

miFocusBorderColor :: String
miFocusBorderColor = "#583ac2" -- Enter a Color Code for the Window's "Focus-State", Default is: "#583ac2";

miBorderWidth :: Dimension
miBorderWidth = 2          -- Enter an Int for the Border Width of the Windows, Default is: "1";

 -- Types of Layouts are Added into this Line: --
miLayout = emptyBSP ||| spiral (6/7) ||| tiled ||| Mirror tiled ||| Full ||| threeCol 
  where
    threeCol
        = renamed [Replace "ThreeCol"]
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    tiled     = Tall nmaster delta ratio
    nmaster   = 1      -- The Default Number of Windows inside of the Master Pane, (Default: 1);
    ratio     = 1/2    -- The Default Proportion of the Screen that Will be Occupied by the Master Pane, (Default: 1/2);
    delta     = 3/100  -- The Percentage of the Screen that Will be Incremented When Resizing Panes, (Default: 3/100);


miMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the Window to Floating Mode and Move by Dragging;
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the Window to the Top of the Stack;
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the Window to Floating mode and Resize by Dragging;
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- You Can Also Bind Events to the Mouse Scroll Wheel (button4 and button5);
    ]

miXmobarPP :: PP
miXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap "[" "]" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap "*" ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    magenta, blue, white, yellow, red, lowWhite, cyan :: String -> String -- Add New Colors to this Strings Line;
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    cyan = xmobarColor  "#59bac9" ""

-- Feel Free to Add Your Own Colors! ^^^^ --

miWorkspaces :: Forest String
miWorkspaces    = [ Node "terminals"
                      [ Node "I" []
                        , Node "II" []
                        , Node "III" []
                      ]
                  , Node "web"
                      [ Node "browser"
                        [ Node "window.1" []
                        , Node "window.2" []
                        , Node "window.3" []
                        ]
                        , Node "discord" []
                        , Node "spotify" []
                        , Node "obs-studio" 
                          [ Node "window.1" [] 
                          , Node "window.2" []
                          ]
                        , Node "other"
                          [ Node "window.1" []
                          , Node "window.2" []
                          , Node "window.3" []
                          ]
                      ]    
                  , Node "godot"
                      [ Node "engine"
                        [ Node "godot-edit" [] 
                        , Node "godot-runtime" []
                        ]
                      , Node "programming"
                          [ Node "neovim.1" []
                          , Node "neovim.2" []
                          , Node "neovim.3" []
                          ]                      
                      , Node "graphic"
                          [ Node "gimp.1" []
                          , Node "gimp.2" []
                          , Node "map-desgin" []
                          ]
                      , Node "audio-production"
                          [ Node "lmms.1" []
                          , Node "lmms.2" []
                          ]
                      ]
                  , Node "gaming"
                      [ Node "steam"
                        [ Node "window.1" []
                        , Node "window.2" []
                        , Node "window.3" []
                        ]
                      , Node "lutris"
                        [ Node "window.1" []
                        , Node "window.2" []
                        , Node "window.3" []
                        ]
                      ]
                  ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "Hello"    "displays hello"      (spawn "xmessage hello!")) []
   , Node (TS.TSNode "Shutdown" "Poweroff the system" (spawn "shutdown")) []
   , Node (TS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TS.TSNode "Bright" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
       , Node (TS.TSNode "Normal" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
       , Node (TS.TSNode "Dim"    "Quite dark"              (spawn "xbacklight -set 10"))  []
       ]
   ]

-- Configuration options for treeSelect
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_font         = "xft:Ubuntu:weight=bold:pixelsize=12:antialias=true:hinting=true"
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = miTreeNavigation
                              }

-- Use h-j-k-l to Navigate;
miTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]

-- P-END, Main Function that Contains the Main Xmonad Functions + Info: --
main :: IO () -- Main Section of the Conifg Where We Define Everything;
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobar/.xmobarrc" (pure miXmobarPP)) toggleStrutsKey
     $ miConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)


miConfig = def
    { terminal            = miTerminal -- Rebinds the "terminal" variable; 
    , modMask             = miModMask    -- Rebinds the Mod-Key to be the Super-Key;
    , startupHook         = miStartupHook -- Rebinds the StartupHook to the pre-configured version;
    , logHook             = workspaceHistoryHook
    , normalBorderColor   = miNormalBorderColor -- Sets the normalBorderColor of Windows;
    , focusedBorderColor  = miFocusBorderColor -- Sets the focusedBorderColor of Windows;
    , borderWidth         = miBorderWidth -- Sets the BorderWidth for Windows;
    , manageHook          = ( isFullscreen --> doFullFloat ) <+> miManageHook <+> manageDocks -- Sets Functions manageHook to doFullFloat and Binds it to miManageHook;
    , workspaces          = TS.toWorkspaces miWorkspaces -- Sets the Workspaces to the Declared Workspaces in p-2;
    , layoutHook          = miLayout -- Sets the Layouts to the Custom Defined Layouts;
    , focusFollowsMouse   = miFocusFollowsMouse -- Sets the Mouse Focus-State to Declared Function;
    }

    `additionalKeysP` -- Binds keybinds: 'M' = Mod-Key, 'S' = Shift, '<>' = Special Keys;
    [ ("M-S-b", spawn "opera"                                                     ) -- Opens Opera;
    , ("M-S-d", spawn "Discord"                                                   ) -- Opens Discord;
    , ("M-S-s", spawn "LD_PRELOAD=/usr/local/lib/spotify-adblock.so spotify"      ) -- Opens Spotify-adblock;
    , ("M-<Space>", spawn "rofi -show drun"                                       ) -- Opens Rofi;
    , ("M-<Return>", spawn miTerminal                                             ) -- Opens Perfered Terminal;
    , ("M-S-g", spawn "gimp"                                                      ) -- Opens Gimp Photo Editor;
    , ("M-g", spawn "./.start-godot"                                              ) -- Launches a Script to open Godot Game Engine;
    , ("M-w", kill                                                                ) -- Kills Targeted Window;
    , ("M-p", sendMessage NextLayout                                              ) -- Switches Between Layouts;
    , ("M-f", sendMessage $ JumpToLayout "Full"                                   ) -- Sets the Layout to Full;
    , ("M-S-f", sendMessage $ JumpToLayout "BSP"                                  ) -- Sets the Layout to emptyBSP;
    , ("M-r", withFocused $ windows . W.sink                                      ) -- Puts Floating Window back into Tiling Mode;
    , ("M-1", windows $ W.greedyView "terminals"                                  )
    , ("M-2", windows $ W.greedyView "web.browser"                               )
    , ("M-3", windows $ W.greedyView "web.browser.window.1"                      )
    , ("M-4", windows $ W.greedyView "web.discord"                                )
    , ("M-5", windows $ W.greedyView "web.spotify"                                )
    , ("M-6", windows $ W.greedyView "gaming.steam"                               )
    , ("M-7", windows $ W.greedyView "gaming.steam.window.1"                      )
    , ("M-8", windows $ W.greedyView "gaming.lutris"                              )
    , ("M-9", windows $ W.greedyView "gaming.lutris.window.1"                     )
    , ("M-0", windows $ W.greedyView "godot.engine.godot-edit"                    )
    , ("M-S-1", windows $ W.shift "terminals"                                     )
    , ("M-S-2", windows $ W.shift "web.browser"                                  )
    , ("M-S-3", windows $ W.shift "web.browser.window.1"                         )
    , ("M-S-4", windows $ W.shift "web.discord"                                   )
    , ("M-S-5", windows $ W.shift "web.spotify"                                   )
    , ("M-S-6", windows $ W.shift "gaming.steam"                                  )
    , ("M-S-7", windows $ W.shift "gaming.steam.window.1"                         )
    , ("M-S-8", windows $ W.shift "gaming.lutris"                                 )
    , ("M-S-9", windows $ W.shift "gaming.lutris.window.1"                        )   
    , ("M-S-0", windows $ W.shift "godot.engine.godot-edit"                       )
    , ("M-t", TS.treeselectWorkspace tsDefaultConfig miWorkspaces W.greedyView    ) -- TreeSelect Workspaces;
    , ("M-S-t", TS.treeselectWorkspace tsDefaultConfig miWorkspaces W.shift       ) -- TreeSelect Move Workspaces;
    , ("M-S-<Return>", windows W.swapMaster                                       ) -- Default Keybind Redefined;
    , ("M-<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"                  ) -- Lower Audio - 5;
    , ("M-<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"                  ) -- Raise Audio + 5;
    , ("M-S-<XF86AudioLowerVolume>", spawn "amixer set Master 10%-"               ) -- Lower Audio - 10;
    , ("M-S-<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+"               ) -- Rasie Audio + 10; 
    , ("<XF86AudioMute>", spawn "amixer set Master toggle"                        ) -- Mute Audio;
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-"                    ) -- Lower Audio - 2;
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+"                    ) -- Raise Audio + 2;
    , ("M-<XF86MonBrightnessUp>", spawn "brightnessctl set +500"                      ) -- Raise Brightness + 10, (Brightnessctl is an option);
    , ("M-<XF86MonBrightnessDown>", spawn "brightnessctl set 500-"                   ) -- Lower Brightness - 10, (Brightnessctl is an option); 
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +200"                         ) -- Raise Brightness, + 5, (Brightnessctl is an option);
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 200-"                       ) -- Lower Brightness, - 5, (Brightnessctl is an option);
    ]
