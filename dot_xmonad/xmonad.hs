import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FindEmptyWorkspace

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio ((%))
import System.IO

main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $
		ewmh $
		docks $
		withUrgencyHook LibNotifyUrgencyHook $
		defaultConfig
		{
		  modMask = mod4Mask
		, terminal = "xfce4-terminal"
		, workspaces = [ "W", "2", "3", "4", "5", "6", "7", "8", "9" ]
		, manageHook = myManageHook <+> manageHook defaultConfig
		, logHook = myLogHook >> dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppSep = "   "
			, ppTitle = shorten 100
			, ppCurrent = wrap "[" "]"
			, ppVisible = wrap "(" ")"
			, ppUrgent = xmobarColor "#eeeeec" "#215d9c"
			, ppOrder = \(ws:_:t:_) -> [ws,t]
 			}
		, layoutHook = myLayoutHook
		, keys = myKeys <+> keys defaultConfig
		, normalBorderColor = "#33393b"
		, focusedBorderColor = "#215d9c"
		}

-- MANAGE HOOK
myManageHook = composeAll
	[
          composeOne [ isFullscreen -?> doFullFloat ]
        , composeOne [ isDialog -?> doCenterFloat ]
        , title =? "VLC (XVideo output)" --> doFullFloat                -- VLC Fullscreen Window
        , className =? "Xmessage" --> doCenterFloat                     -- X Messages
        , className =? "Mixxx" --> doCenterFloat                        -- Mixxx DJ Console
        , className =? "Xfrun4" --> doFloat                             -- Xfce-Run Dialog
        , className =? "Xfce4-notes-plugin" --> doFloat                 -- Xfce Panel Notes Plugin
        , className =? "Xfce4-mixer" --> doFloat                        -- Xfce Mixer Application
        , className =? "Gcalctool" --> doFloat                          -- Calculator
        , className =? "Totem" --> doFloat                              -- Totem Video Player
        , className =? "Vlc" --> doFloat                                -- VLC Media Player (normal)
        , className =? "Keepassx" --> doFloat                           -- KeepassX Password Manager
        , className =? "Xfce4-notifyd" --> doIgnore                     -- Ignore Popup Notifications
        , className =? "Wrapper-1.0" --> doFloat                        -- Whiskermenu popup and other popup dialogs
	, className =? "Firefox" --> doShift "W"			-- Firefox
	, className =? "qutebrowser" --> doShift "W"			-- Qutebrowser
	]


-- LAYOUT HOOK
myLayoutHook = avoidStruts $ noBorders $ webLayout $ myThreeCol
	where
	webLayout = onWorkspace "W" myWebLayout

myThreeCol = ifWider 1920 (myDefaultLayout ||| ThreeColMid nmaster delta ratio) myDefaultLayout
        where
        nmaster = 2
        delta = 3/100
        ratio = 1/3

myWebLayout = ifWider 1920 (Full ||| myTwoColLayout) Full

myDefaultLayout = myTwoColLayout ||| Mirror myTwoColLayout ||| Full

myTwoColLayout = Tall 1 (3/100) (1/2)


-- LOG HOOK
myLogHook = fadeInactiveLogHook fadeAmount >> logHook defaultConfig >> updatePointer (0.5, 0.5) (0, 0)
	where
	fadeAmount = 0.8


-- PRETTY PRINT LAYOUTS
notifyLayoutPP = def { ppOrder = \(_:l:_:_) -> [l] }


-- KEYS
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm, xK_f), fullFloatFocused)
	, ((modm, xK_p), spawn "$HOME/.xmonad/setup-monitors.sh")
	, ((modm, xK_b), sendMessage ToggleStruts)
	, ((modm, xK_n), viewEmptyWorkspace)
	, ((modm .|. shiftMask, xK_n), tagToEmptyWorkspace) 
	, ((modm .|. shiftMask, xK_p), spawn "systemctl poweroff")
	, ((modm .|. shiftMask, xK_l), spawn "slock xset dpms force off")
--        , ((modm .|. shiftMask, xK_BackSpace), spawn "xfce4-terminal --command ranger --initial-title=Ranger")
        , ((modm .|. shiftMask, xK_BackSpace), spawn "thunar")
        , ((modm .|. shiftMask, xK_h), spawn "xfce4-terminal --command htop --initial-title=htop")
        , ((modm .|. shiftMask, xK_m), spawn "xfce4-terminal --command mc --initial-title='Midnight Commander'")
	, ((modm, xK_g), windowPromptGoto myPromptConfig)
	, ((modm, xK_x), shellPrompt myPromptConfig)
	, ((modm, xK_space), sendMessage NextLayout >> (dynamicLogString notifyLayoutPP >>= \d->spawn $"notify-send -u low \"" ++ d ++ "\""))
	, ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
	, ((modm, xK_Left), prevWS)
	, ((modm, xK_Right), nextWS)
	, ((0, xF86XK_Forward), nextWS)
	, ((0, xF86XK_Back), prevWS)
	]


-- PROMPT CONFIGURATION
myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
        { position = Bottom
        , promptBorderWidth = 0
      , font = "xft:Ubuntu Mono:style=Regular,size=12"
      , height = 24
      , bgColor = "#33393b"
      , fgColor = "#eeeeec"
      , bgHLight = "#215d9c"
      , fgHLight = "#ffffff"
      , autoComplete = Just 500000
--      , autoComplete = Nothing
        }


-- MISC

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [ show name, "on workspace " ++ idx ]


fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

