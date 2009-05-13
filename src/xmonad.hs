import XMonad
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Util.WindowProperties
import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import qualified XMonad.Layout.PerWorkspace as PW
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
bitmaps_dir             = "/home/bjs/.xmonad"

---------------------
-- Execute Strings --
---------------------

commonBarFont    = "-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*"
wmInfoBarFgColor = "#FFFFFF"
wmInfoBarBgColor = "#333538"
wmInfoBarWidth   = "1024"
wmInfoBarScreen  = "1"
wmInfoBarCmd     = "dzen2 -ta l -dock -h 20 -geometry -0+0"
                        ++ " -w " ++ wmInfoBarWidth
                        ++ " -bg '" ++ wmInfoBarBgColor ++ "'"
                        ++ " -fg '" ++ wmInfoBarFgColor ++ "'"
                        ++ " -fn '" ++ commonBarFont    ++ "'"
                        ++ " -e 'onstart=lower'"
                        ++ " -xs " ++ wmInfoBarScreen
-- can't use this b/c I don't want to relaunch everything on a reload of my xmonad.hs
initCmd         = "/bin/sh ~/.xmonad/init.sh"

-----------------
-- KeyBindings --
-----------------

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

-----------------------
-- Window Management --
-----------------------

myManageHook :: ManageHook
myManageHook = composeAll
    -- Legend:
    --      sec1 -> rules for floating windows
    --      sec2 -> rules for ignoring windows
    --      sec3 -> rules for moving windows to workspaces
    -- sec1 --
    [ className =? "XFontSel"                           --> doCenterFloat
    , className =? "Restart Firefox"                    --> doCenterFloat
    , title     =? "Firefox - Restore Previous Session" --> doFloat
    , title     =? "Firefox Preferences"                --> doFloat
    -- sec2 --
    , className =? "stalonetray"                        --> doIgnore
    -- sec3 --
    , className =? "Firefox"                            --> doF (S.shift "web")
    , className =? "Opera"                              --> doF (S.shift "web")
    , className =? "opera"                              --> doF (S.shift "web")
    ]

myOtherManageHook = composeOne [ isFullscreen -?> doFullFloat ]

-------------------
-- Layout Config --
-------------------
--
----- Sources
--
--  [1]: http://haskell.org/haskellwiki/Xmonad/Config_archive/enko's_xmonad.hs
--  [2]: http://haskell.org/haskellwiki/Xmonad/Config_archive/andrewsw's_xmonad.hs_(0.8)
--  [3]: http://haskell.org/haskellwiki/Xmonad/Config_archive/loupgaroublonds_xmonad.hs
--
myLayouts = smartBorders . avoidStruts $ comm all
    where
    -- Legend:
    --      comm    => set of layouts to use on the 'comm' workspace
    --      all     => set of layouts to use on the non-'limited' workspaces
    --      tiled   => the 'Tall' layout called with non-default options
    --      nmaster => the default # of windows in the master pane
    --      delta   => the percent to +/- when resizing panes
    --      ratio   => the proportion of the screen owned by the master pane
    comm    = PW.onWorkspace "comm" ( Tall nmaster delta (71/100) ||| Full )
    all     = tiled ||| Mirror tiled ||| Full
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

----------------
-- Custom Log --
--   Output   --
----------------
-- I want to move the logHook definition here if possible
myDzenPP dzfh = defaultPP
            { ppCurrent             = wrap "^fg(#000000)^bg(#a3ef5d) " " ^fg()^bg()"
            , ppVisible             = wrap "^bg(grey30)^fg(grey75) " " ^fg()^bg()"
            , ppHidden              = wrap ("^i(" ++ bitmaps_dir ++ "/has_win_nv.xbm)") " "
            , ppHiddenNoWindows     = wrap " " " "
            , ppSep                 = " ^r(3x3) "
            , ppWsSep               = ""
            , ppLayout              = (\x -> case x of
                                                "Tall"        -> "^i(" ++ bitmaps_dir ++ "/tall.xbm)"
                                                "Mirror Tall" -> "^i(" ++ bitmaps_dir ++ "/mtall.xbm)"
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
    dzenproc <- spawnPipe wmInfoBarCmd
    xmonad $ defaultConfig
        { keys                  = myKeysFunc
        , terminal              = myTerminal
        , workspaces            = myWorkspaces
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , modMask               = myModMask
        , layoutHook            = myLayouts
        , manageHook            = manageDocks <+> myOtherManageHook <+> myManageHook <+> manageHook defaultConfig
        , logHook               = dynamicLogWithPP $ myDzenPP dzenproc
        }
