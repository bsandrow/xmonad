import XMonad
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Util.WindowProperties
import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders

import Control.Monad
import qualified XMonad.StackSet as S
import qualified Data.Map as M
import System.IO

--------------------
-- Customizations --
--------------------

myTerminal              = "term"
myBorderWidth           = 4
myNormalBorderColor     = "#202030"
myFocusedBorderColor    = "#0A0AD0"
myWorkspaces            = ["web","comm"] ++ map show [3..6]
fullFloatOn             = 0

---------------------
-- Execute Strings --
---------------------

commonBarFont   = "-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*"
upperBarFgColor = "#FFFFFF"
upperBarBgColor = "#333538"
lowerBarFgColor = "#FFFFFF"
lowerBarBgColor = "#111321"
upperBarCmd     = "dzen2 -ta l -dock -h 20 -geometry -0+0   -bg '" ++ upperBarBgColor ++ "' -fg '" ++ upperBarFgColor ++ "' -fn '" ++ commonBarFont ++ "'" 
lowerBarCmd     = "bash ~/scripts/xmonad-bottom-bar.sh | dzen2 -ta l -dock -h 14 -geometry +0+754 -bg '" ++ lowerBarBgColor ++ "' -fg '" ++ lowerBarFgColor ++ "' -fn '" ++ commonBarFont ++ "'"
trayKillCmd     = "kill -9 `ps -C stalonetray -o pid,user h|grep bjs|sed 's:^ *::g'|cut -d' ' -f1|awk '{ print $1\" \"; }'`"
trayCmd         = "sleep 1 && stalonetray"

------------------------
-- Custom KeyBindings --
------------------------

myModMask = mod4Mask
altMask   = mod1Mask

myKeys conf =
    [ ((myModMask,              xK_Return), spawn $ XMonad.terminal conf)       -- launch terminal
    , ((myModMask,              xK_c     ), spawn $ "gmrun")                    -- launch gmrun
    , ((altMask,                xK_Tab   ), windows S.focusDown)                -- cycle down from window
    , ((altMask .|. shiftMask,  xK_Tab   ), windows S.focusUp)                  -- cycle up from window
    , ((myModMask,              xK_Down  ), windows S.swapDown)                 -- move window down in layout
    , ((myModMask,              xK_Up    ), windows S.swapUp)                   -- move window up in layout
    , ((myModMask,              xK_m     ), windows S.swapMaster)               -- swap cur window w/ master window
    , ((myModMask,              xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True) -- restart xmonad
    , ((myModMask,              xK_space ), sendMessage NextLayout)             -- cycle through layouts
    , ((myModMask,              xK_Tab   ), nextWS)                             -- cycle forward 1 workspace
    , ((myModMask .|. shiftMask,xK_Tab   ), prevWS)                             -- cycle back 1 workspace
    , ((myModMask,              xK_x     ), kill)                               -- close current window
--  , ((myModMask,              xK_f     ), withFocused $ doFullFloat)          -- de-float the window
    , ((myModMask .|. shiftMask,xK_l     ), spawn "slock")                      -- run 'slock' to lock the screen
    , ((myModMask .|. altMask,  xK_Tab   ), nextScreen)                         -- cycle to the next screen
    ] ++
    [ ((altMask, k), windows $ S.greedyView i)
        | (i,k) <- zip myWorkspaces workspaceKeys
    ] ++
    [ ((myModMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
        | (i,k) <- zip myWorkspaces workspaceKeys
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]

myKeysFunc x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)

------------------------------
-- Custom Window Management --
------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    -- rules to float windows outside of tiling management
    [ className =? "XFontSel"                           --> doCenterFloat
    , className =? "Restart Firefox"                    --> doCenterFloat
    , title     =? "Firefox - Restore Previous Session" --> doFloat
    , title     =? "Firefox Preferences"                --> doFloat
    -- rules to exclude windows from management
    , className =? "stalonetray"                        --> doIgnore
    -- rules to automatically move windows to certain workspaces
    , className =? "Firefox"                            --> doF (S.shift "web")
    , className =? "Opera"                              --> doF (S.shift "web")
    , className =? "opera"                              --> doF (S.shift "web")
    ]

myOtherManageHook = composeOne [ isFullscreen -?> doFullFloat ]

----------------
-- Custom Log --
--   Output   --
----------------
-- I want to move the logHook definition here if possible
myDzenPP dzfh = defaultPP
            { ppCurrent             = wrap "^fg(#000000)^bg(#a3ef5d) " " ^fg()^bg()"
            , ppVisible             = wrap "^bg(grey30)^fg(grey75)" "^fg()^bg()"
            , ppHidden              = wrap "^i(/home/bjs/local/dzen_bitmaps/has_win_nv.xbm)" " "
            , ppHiddenNoWindows     = wrap " " " "
            , ppSep                 = " ^r(3x3) "
            , ppWsSep               = ""
            , ppLayout              = (\x -> case x of
                                                "Tall"        -> "^i(/home/bjs/local/dzen_bitmaps/tall.xbm)"
                                                "Mirror Tall" -> "^i(/home/bjs/local/dzen_bitmaps/mtall.xbm)"
                                                x             -> x
                                      )
            , ppTitle               = dzenColor "#a3ef5d" "" . shorten 80
--            , ppTitle               = wrap "<^fg(#a3ef5d) " " ^fg()>"
            , ppOutput              = hPutStrLn dzfh
            }

------------------------------
-- Main Function Definition --
------------------------------
main = do
    dzenproc <- spawnPipe upperBarCmd
    proc1    <- spawnPipe trayKillCmd
    proc2    <- spawnPipe trayCmd
    proc3    <- spawnPipe lowerBarCmd
    xmonad $ defaultConfig
        { keys                  = myKeysFunc
        , terminal              = myTerminal
        , workspaces            = myWorkspaces
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , modMask               = myModMask
        , layoutHook            = avoidStruts $ layoutHook defaultConfig
        , manageHook            = manageDocks <+> myOtherManageHook <+> myManageHook <+> manageHook defaultConfig
        , logHook               = dynamicLogWithPP $ myDzenPP dzenproc
        }
