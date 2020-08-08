import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import System.IO
import XMonad.Util.SpawnOnce
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import Graphics.X11.ExtraTypes.XF86

myTerminal :: String
myTerminal = "alacritty"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod4Mask

myBrowser :: String
myBrowser = "google-chrome-stable"

myWorkspaces = ["main", "web", "code", "media", "notes", "chat", "7", "8", "9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#292d3e"
myFocusedBorderColor = "#bbc5ff"

myLayout = avoidStruts (defaultTall ||| Mirror tiled ||| Grid ||| Full)
    where
        tiled = Tall nmaster delta ratio
        defaultTall = ResizableTall nmaster delta ratio []
        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio = 1/2

        -- Percent of screen to increment by when resizing panes
        delta = 3/100

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom &"
    spawnOnce "trayer --edge top --iconspacing 0 --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 22 &"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

myKeys :: [(String, X ())]
myKeys =  [ 
              ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
            , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@  -1.5%")
            , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")    

            , ("<XF86AudioPlay>", spawn "playerctl play-pause")    
            , ("<XF86AudioPrev>", spawn "playerctl previous")    
            , ("<XF86AudioNext>", spawn "playerctl next")    

            , ("<XF86MonBrightnessUp>", spawn "lux -a 5%")    
            , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
            , ("M-S-z", spawn "xscreensaver-command -lock; xset dpms force off")
            , ("M-<Print>", spawn "flameshot screen -c")
            , ("<Print>", spawn "flameshot gui")
            , ("M-p", spawn "rofi -show combi")

            , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
            , ("M-C-k", sendMessage MirrorExpand)               -- Expand vert window width
            ]
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/blake/.xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 /home/blake/.xmobarrc"
    xmproc2 <- spawnPipe "xmobar -x 2 /home/blake/.xmobarrc"
    
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout 
        , startupHook = myStartupHook
        , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
                        , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        },
            borderWidth = myBorderWidth,
            terminal = myTerminal,
            workspaces = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeysP` myKeys
        
