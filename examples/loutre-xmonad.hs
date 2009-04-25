-- [[ snagged from http://dotfiles.org/~loutre/xmonad.hs ]] --

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

--XMonad.Layout.IM
--XMonad.Layout.Magnifier
--XMonad.Layout.Roledex
--XMonad.Layout.simpleDeco
--XMonad.Layout.simpleFloat
--

myModMask = mod1Mask
myManageHook = composeAll [
    className =? "Gimp" --> doFloat,
    className =? "Pidgin" --> doFloat,
    className =? "Wine" --> doFloat,
    className =? "Gpicview" --> doFloat,
    className =? "OpenOffice.org 3.0" --> doFloat,
    className =? "MPlayer" --> doFloat]

myOtherManageHook = composeOne [ isFullscreen -?> doFullFloat ] 


-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#20bbff"

-- Control Center {{{
-- Colour scheme {{{
myNormalBGColor     = myNormalBorderColor
myFocusedBGColor    = myNormalBorderColor
myNormalFGColor     = "#dcdcdc"
myFocusedFGColor    = myFocusedBorderColor
myUrgentFGColor     = "#ffffff"
myUrgentBGColor     = myFocusedBorderColor
mySeperatorColor    = "#dcdcdc"
-- }}}
-- Icon packs can be found here:
-- http://robm.selfip.net/wiki.sh/-main/DzenIconPacks
myBitmapsDir        = "/home/alex/.dzen"
myFont              = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-iso8859-1"

myBorderWidth   = 2

-- }}}

-- Statusbar options:
myStatusBar = "dzen2 -x '0' -y '0' -h '14' -w '1244' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "zsh ~/.xmonad/dzentop.sh | dzen2 -x '1244' -y '0' -h '14' -w '200' -ta 'r' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myBottomBar = "zsh ~/.xmonad/dzenbottom.sh | dzen2 -x '0' -y '884' -h '16' -w '1296' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

my9Menu = "9menu -popup -bg '" ++ myNormalBGColor ++ "' -fg '" ++ myFocusedFGColor ++ "' -teleport -file /home/alex/.xmonad/9menu -fname '" ++ myFont ++ "'"
myDmenu = "exe=`dmenu_path | dmenu -fn '" ++ myFont ++ "' -nb '" ++ myNormalBGColor ++ "' -nf '" ++ myNormalFGColor ++ "' -sb '" ++ myFocusedFGColor ++ "' -sf '" ++ myNormalFGColor ++ "'` && eval \"exec $exe\""
myInit = "/bin/sh /home/alex/.xmonad/init.sh"

myLayout = avoidStruts (smartBorders (tiled ||| Mirror tiled ||| ThreeCol 1 (3/100) (1/2) ||| spiral (6/7) ||| Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Volé d'un voleur, mais modifié
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/Xilon's_xmonad.hs
-- Dzen Pretty Printer {{{
-- Stolen from Rob [1] and modified
-- [1] http://haskell.org/haskellwiki/Xmonad/Config_archive/Robert_Manea%27s_xmonad.hs
myPP handle = defaultPP {
        ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myUrgentBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppHidden = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppSep     = " ^fg(" ++ mySeperatorColor ++ ")^r(3x3)^fg() ",
        ppLayout  = (\x -> case x of
                    "Tall"          -> " ^i(" ++ myBitmapsDir ++ "/tall.xbm) "
                    "Mirror Tall"   -> " ^i(" ++ myBitmapsDir ++ "/mtall.xbm) "
                    "Full"          -> " ^i(" ++ myBitmapsDir ++ "/full.xbm) "
                    "ThreeCol"      -> " ^i(" ++ myBitmapsDir ++ "/threecol.xbm) "
                    "Spiral"        -> " ^i(" ++ myBitmapsDir ++ "/spiral.xbm) "
                    _               -> " " ++ x ++ " "
                ),
        ppTitle   = wrap ("^fg(" ++ myFocusedFGColor ++ ")") "^fg()" ,
        ppOutput  = hPutStrLn handle
}


myUrgencyHook = withUrgencyHook dzenUrgencyHook { args = ["-bg", myFocusedFGColor, "-fg", myNormalFGColor, "-fn", myFont, "-h", "14"] }

-- Plusieurs barres lancés par xmonad
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/And1's_xmonad.hs
-- Vieille ligne:
-- xmproc <- spawnPipe "xmobar /home/alex/.xmonad/xmobar"
--
main = do
  din <- spawnPipe myStatusBar
  din2 <- spawnPipe myBottomBar
  din3 <- spawnPipe myTopBar

  xmonad $ myUrgencyHook defaultConfig {
         terminal   = "urxvt",
         workspaces         = myWorkspaces,
         normalBorderColor  = myNormalBorderColor,
         focusedBorderColor = myFocusedBorderColor,
	 borderWidth        = myBorderWidth,
	 startupHook = setWMName "LG3D",
	 focusFollowsMouse  = myFocusFollowsMouse,

         manageHook = manageDocks <+> myManageHook <+> myOtherManageHook <+> manageHook defaultConfig,
         layoutHook = myLayout,
	 logHook = dynamicLogWithPP $ myPP din,
	 modMask = myModMask
         } `additionalKeys`
	 [((myModMask .|. shiftMask, xK_z),
           spawn "xscreensaver-command -lock"),
          ((0, xK_Print), spawn "scrot"),
	  ((myModMask, xK_p), spawn myDmenu),
	  ((myModMask, xK_c), spawn "~/.xmonad/dzencal.sh")
         ]

