import XMonad
import Data.Monoid
import Data.Tree
import System.Exit
import qualified Data.Map as M
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig 
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.SessionStart
import XMonad.Util.Run
import XMonad.Util.Loggers

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Util.ClickableWorkspaces 
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ServerMode

import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed 
import XMonad.Layout.LayoutCombinators

import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CopyWindow 
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import qualified XMonad.StackSet as W

miStartupHook :: X ()
miStartupHook = do     
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom --daemon"
    spawnOnce "emacs --daemon &"
    spawnOnce "dunst &"

miManageHook = composeAll
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
miTerminal = "alacritty"

miBrowser :: String
miBrowser = "brave-browser"

miModMask :: KeyMask
miModMask = mod4Mask

miFocusFollowsMouse :: Bool
miFocusFollowsMouse = False

miNormalBorderColor :: String
miNormalBorderColor = "#6655a3"

miFocusBorderColor :: String
miFocusBorderColor = "#583ac2"

miBorderWidth :: Dimension
miBorderWidth = 2

miLayout = tiled ||| Mirror tiled ||| Full ||| threeCol 
  where
    threeCol
	= renamed [Replace "ThreeCol"]
	$ magnifiercz' 1.3
	$ ThreeColMid nmaster delta ratio
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

miMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
				       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
				       >> windows W.shiftMaster))
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

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    magenta, blue, white, yellow, red, lowWhite, cyan :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    cyan     = xmobarColor  "#59bac9" ""

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobar/.xmobarrc" (pure miXmobarPP)) toggleStrutsKey
     $ miConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)


miConfig = def
    { terminal            = miTerminal 
    , modMask             = miModMask
    , startupHook         = miStartupHook
    , logHook             = workspaceHistoryHook
    , normalBorderColor   = miNormalBorderColor
    , focusedBorderColor  = miFocusBorderColor
    , borderWidth         = miBorderWidth
    , manageHook          = ( isFullscreen --> doFullFloat ) <+> miManageHook <+> manageDocks
    , layoutHook          = miLayout
    , focusFollowsMouse   = miFocusFollowsMouse
    }

    `additionalKeysP`
    [ ("M-S-b", spawn "brave"                                                     )
    , ("M-S-d", spawn "discord"                                                   )
    , ("M-<Space>", spawn "dmenu_run"                                             )
    , ("M-<Return>", spawn miTerminal                                             )
    , ("M-S-g", spawn "gimp"                                                      )
    , ("M-g", spawn "godot"				                          )
    , ("M-e", spawn "emacsclient -c"                                              )
    , ("M-w", kill                                                                )
    , ("M-p", sendMessage NextLayout                                              )
    , ("M-f", sendMessage $ JumpToLayout "Full"                                   )
    , ("M-S-f", sendMessage $ JumpToLayout "tiled"                                )
    , ("M-m", sendMessage $ JumpToLayout "Mirror tiled"		                  )
    , ("M-S-<Space>", withFocused $ windows . W.sink                              )
    , ("M-S-<Return>", windows W.swapMaster                                       )
    , ("<XF86AudioMute>", spawn "amixer set Master toggle"                        )
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-"                    )
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+"                    )
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +200"                    )
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 200-"                  )
    ]
