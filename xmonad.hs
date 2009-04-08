import XMonad
import XMonad.ManageHook
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(doCenterFloat)
import XMonad.Util.WindowProperties
import XMonad.Layout.WindowNavigation
import Control.Monad
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as S
import qualified Data.Map as M
import System.IO

-------------------------
-- Simple              --
--    Variable         --
--      Customizations --
-------------------------
myTerminal              = "term"
myBorderWidth           = 2
myNormalBorderColor     = "#202030"
myFocusedBorderColor    = "#A0A0D0"
myWorkspaces            = map show [1..9]
statusCmd               = "dzen2 -e 'onstart=lower' -ta l -dock -bg \"#333538\" -h 20 -fg \"#FFFFFF\" -fn '-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*'  -geometry -0+0"

-----------------
--    Custom   --
-- KeyBindings --
-----------------

myModMask = mod4Mask
altMask   = mod1Mask

myKeys conf =
    [ ((myModMask,              xK_Return), spawn $ XMonad.terminal conf)       -- launch terminal
    , ((myModMask,              xK_p     ), spawn $ "gmrun")                    -- launch gmrun
    , ((altMask,                xK_Tab   ), windows S.focusDown)                -- cycle down from window
    , ((altMask .|. shiftMask,  xK_Tab   ), windows S.focusUp)                  -- cycle up from window
    , ((myModMask,              xK_Down  ), windows S.swapDown)                 -- move window down in layout
    , ((myModMask,              xK_Up    ), windows S.swapUp)                   -- move window up in layout
    , ((myModMask,              xK_m     ), windows S.swapMaster)               -- swap cur window w/ master window
    , ((myModMask,              xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True) -- restart xmonad
    , ((myModMask,              xK_space ), sendMessage NextLayout)             -- cycle through layouts
    , ((myModMask,              xK_Tab   ), nextWS)                             -- cycle forward 1 workspace
    , ((myModMask .|. shiftMask,xK_Tab   ), prevWS)                             -- cycle back 1 workspace
    , ((myModMask,              xK_c     ), kill)                               -- close current window
    , ((myModMask,              xK_f     ), withFocused $ windows . S.sink)     -- de-float the window
    , ((myModMask,              xK_l     ), spawn "xscreensaver-command -lock")                      -- run 'slock' to lock the screen
    ] ++
    [ ((altMask, k), windows $ S.greedyView i)
        | (i,k) <- zip myWorkspaces workspaceKeys
    ] ++
    [ ((myModMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
        | (i,k) <- zip myWorkspaces workspaceKeys
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]

myKeysFunc x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)


-- Mouse

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    ]
----------------
--   Custom   --
--   Window   --
-- Management --
----------------
--
-- myFloatsByClass      : list of window classes to match against for floating
-- myFloatsByTitle      : list of window titles to match against for floating
-- myCenterFloatsByClass: list of window clases to float _and_ center on the screen
--
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [className =? c --> doFloat       | c <- myFloatsByClass       ]
    , [title     =? t --> doFloat       | t <- myFloatsByTitle       ]
    , [className =? c --> doCenterFloat | c <- myCenterFloatsByClass ]
    , [className =? c --> doIgnore      | c <- myIgnoreByClass       ]
--  , [className =? "Firefox" && title =? "Save As" --> doCenterFloat]
    ]
    where
    myFloatsByClass       = []
    myFloatsByTitle       = [ "Firefox - Restore Previous Session"
                            , "Firefox Preferences"
                            ]
    myCenterFloatsByClass = [ "XFontSel"
                            , "Restart Firefox"
                            ]
    myIgnoreByClass       = [ "stalonetray"
                            ]

----------------
-- Custom Log --
--   Output   --
----------------
                                                         -- #a6c292
-- I want to move the logHook definition here if possible
myDzenPP dzfh = defaultPP
            { ppCurrent             = wrap "^fg(#000000)^bg(#A0A0D0) " " ^fg()^bg()"
            , ppHidden              = wrap "^i(/home/bjs/local/dzen_bitmaps/has_win_nv.xbm)" " "
            , ppHiddenNoWindows     = wrap " " " "
            , ppSep                 = " ^r(3x3) "
            , ppWsSep               = ""
            , ppLayout              = (\x -> case x of
                                                "Tall"   -> "^i(/home/bjs/local/dzen_bitmaps/tall.xbm)"
                                                "Mirror" -> "^i(/home/bjs/local/dzen_bitmaps/mtall.xbm)"
                                                x        -> x
                                      )
            , ppTitle               = wrap "< " " >" 
            , ppOutput              = hPutStrLn dzfh
            }

-------------------
-- Main          --
--  Function     --
--    Definition --
-------------------
main = do
    dzenproc <- spawnPipe statusCmd
    xmonad $ defaultConfig
        { keys                  = myKeysFunc
        , terminal              = myTerminal
        , workspaces            = myWorkspaces
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , modMask               = myModMask
        , layoutHook            = avoidStruts $ layoutHook defaultConfig
        , manageHook            = myManageHook <+> manageHook defaultConfig <+> manageDocks
        , logHook               = dynamicLogWithPP $ myDzenPP dzenproc
        , mouseBindings         = myMouseBindings
        }
