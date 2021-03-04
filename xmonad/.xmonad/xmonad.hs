
import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName

import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances


import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Data.Char
import Data.Monoid

import System.IO
import System.Exit

import qualified DBus                     as D
import qualified DBus.Client              as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet          as W
import qualified Data.Map                 as M

-- 10 workspaces should be enough
ws = ["1","2","3","4","5","6","7","8","9","0"]

fontFamily = "xft:FantasqueSansMono Nerd Font:size=10:antialias=true:hinting=true"
fontFamilyLarge = "xft:FantasqueSansMono Nerd Font:size=16:style=Bold:antialias=true:hinting=true"

keybindings =
  [ ("M-<Return>",                 spawn "kitty")
  , ("M-d",                        shellPrompt promptConfig)
  , ("M-a",                        spawn "emacs")
  , ("M-o",                        spawn "librewolf")
---  , ("M-e",                        sendMessage ToggleLayout)
  , ("M-<Tab>",                    sendMessage NextLayout)
  , ("M-n",                        refresh)
  , ("M-s",                        windows W.swapMaster)
  , ("M--",                        sendMessage Shrink)
  , ("M-=",                        sendMessage Expand)
  ,("M-S-j",                       windows W.swapDown)
  , ("M-S-k",                      windows W.swapUp)  
  , ("M-t",                        withFocused toggleFloat)
  , ("M-,",                        sendMessage (IncMasterN 1))
  , ("M-.",                        sendMessage (IncMasterN (-1)))
  , ("M-S-q",                      io (exitWith ExitSuccess))
  , ("M-S-c",                      withFocused $ \w -> spawn ("xkill -id " ++ show w))
  , ("M-S-r",                      spawn $ "xmonad --restart && systemctl --user restart polybar")
  , ("M-S-<Left>",                 shiftToPrev >> prevWS)
  , ("M-S-<Right>",                shiftToNext >> nextWS)
  , ("M-j",                        windows W.focusUp)
  , ("M-k",                        windows W.focusDown)
  , ("M-S-<Tab>",                  sendMessage FirstLayout)

  ]
  ++
  [ (otherModMasks ++ "M-" ++ key, action tag)
      | (tag, key) <- zip ws (map (\x -> show x) ([1..9] ++ [0]))
      , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                   , ("S-", windows . W.shift)]
  ]
  where
    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                              then W.sink w s
                              else (W.float w (W.RationalRect 0.15 0.15 0.7 0.7) s))

promptConfig = def
  { font                = fontFamily
  , bgColor             = "#16161c"
  , fgColor             = "#fdf0ed"
  , bgHLight            = "#e95678"
  , fgHLight            = "#16161c"
  , borderColor         = "#e95678"
  , promptBorderWidth   = 0
  , position            = Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , showCompletionOnTab = False
  , searchPredicate     = fuzzyMatch
  , sorter              = fuzzySort
  , defaultPrompter     = id $ map toLower
  , alwaysHighlight     = True
  , maxComplRows        = Just 5
  }

layouts = avoidStruts $ (tiled ||| Mirror tiled ||| tabs ||| centeredMaster ||| grid) ||| Full
  where
     tiled = gaps 4 4 $ toggleLayouts maximized (smartBorders (Tall 1 (3/100) (1/2)))
     centeredMaster = gaps 4 4 $ toggleLayouts maximized (smartBorders (ThreeColMid 1 (3/100) (1/2)))
     tabs = gaps 8 0 $ noBorders (tabbed shrinkText tabTheme)
     grid = gaps 4 4 $ toggleLayouts maximized (smartBorders Grid)
     maximized = smartBorders Full
     gaps n k = spacingRaw False (Border n n n n) True (Border k k k k) True

tabTheme = def
  { fontName            = fontFamily
  , activeColor         = "#e95678"
  , inactiveColor       = "#16161c"
  , urgentColor         = "#ee64ae"
  , activeTextColor     = "#16161c"
  , inactiveTextColor   = "#fdf0ed"
  , urgentTextColor     = "#16161c"
  , activeBorderWidth   = 0
  , inactiveBorderWidth = 0
  , urgentBorderWidth   = 0
  }

wnameTheme = def
  { swn_font    = fontFamilyLarge
  , swn_bgcolor = "#e95678"
  , swn_color   = "#16161c"
  , swn_fade    = 2
  }

windowRules = placeHook (smart (0.5, 0.5))
  <+> composeAll
  [ className  =? "Gimp"                                   --> doFloat
  , (className =? "Ripcord" <&&> title =? "Preferences")   --> doFloat
  , className  =? "Xmessage"                               --> doFloat
  , className  =? "Krita"                                  --> doFloat
  , className  =? "Xcalc"                                  --> doFloat
  , resource   =? "desktop_window"                         --> doIgnore
  , resource   =? "kdesktop"                               --> doIgnore
  , isDialog                                               --> doF W.swapUp <+> doFloat ]
  <+> insertPosition End Newer -- same effect as attachaside patch in dwm
  <+> manageDocks
  <+> manageHook defaultConfig

autostart = do
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "dunst &"
  spawnOnce "bash ~/.config/polybar/launch.sh &"
  spawnOnce "mpd &"
  spawnOnce "picom &"
  spawnOnce "wal -i $HOME/Pictures/wallpapers/"
  setWMName "xmonad"

dbusClient = do
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.log") opts
    return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput dbus str =
  let
    opath  = D.objectPath_ "/org/xmonad/Log"
    iname  = D.interfaceName_ "org.xmonad.Log"
    mname  = D.memberName_ "Update"
    signal = D.signal opath iname mname
    body   = [D.toVariant $ UTF8.decodeString str]
  in
    D.emit dbus $ signal { D.signalBody = body }

polybarHook dbus = dynamicLogWithPP $ xmobarPP
  { ppOutput = dbusOutput dbus
  , ppOrder  = \(_:l:_:_) -> [l]
  }

main' dbus = xmonad . docks . ewmh $ def
  { focusFollowsMouse  = True
  , clickJustFocuses   = True
  , borderWidth        = 2
  , modMask            = mod4Mask
  , workspaces         = ws
  , normalBorderColor  = "#2e303e"
  , focusedBorderColor = "#e95678"
  , layoutHook         = showWName' wnameTheme layouts
  , manageHook         = windowRules
  , logHook            = fadeInactiveLogHook 0.95 <+> polybarHook dbus
  , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHook
  , startupHook        = autostart
  } `additionalKeysP` keybindings

main = dbusClient >>= main' -- "that was easy, xmonad rocks!"

