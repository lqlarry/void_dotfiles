------------------------------------------------------------------------
   -- Imports
------------------------------------------------------------------------
   -- Base
import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)  
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Actions.PhysicalScreens
import XMonad.Util.NamedScratchpad

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, pad, xmobar, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat) 
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)

import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace) 
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt ( XPConfig(..), XPPosition(Top), Direction1D(..))
-- import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont          = "xft:Hurmit Nerd Font Mono:regular:pixelsize=12"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "alacritty"      -- Sets default terminal
myTextEditor    = "nvim"     -- Sets default text editor
myBorderWidth   = 4         -- Sets border width for windows
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
main = do
        barpipe <- spawnPipe "xmobar -x 0 /home/larry/.config/xmobar/xmobarrc"
        xmonad $ ewmh desktopConfig
          { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
          , logHook = dynamicLogWithPP xmobarPP
                         {ppCurrent = xmobarColor "#8be9fd" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppOutput = hPutStrLn barpipe
                        , ppVisible = xmobarColor "#8be9fd" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#a3be8c" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#d8dee9" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#d8dee9> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
          , modMask            = myModMask
          , terminal           = myTerminal
          , startupHook        = myStartupHook
          , layoutHook         = myLayoutHook 
          , workspaces         = myWorkspaces
          , borderWidth        = myBorderWidth
          , normalBorderColor  = "#d8dee9"
          , focusedBorderColor = "#5e81ac"
          } `additionalKeysP`         myKeys 

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
          spawnOnce "/home/larry/.config/xmonad/scripts/autostart.sh &"

------------------------------------------------------------------------
---SCRATCHPADS
------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp
                , NS "ranger" spawnRanger findRanger manageRanger
                , NS "bitwarden" spawnBitwarden findBitwarden manageBitwarden
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnNcmpcpp  = myTerminal ++ " -t ncmpcpp -e ncmpcpp"
    findNcmpcpp   = title =? "ncmpcpp"
    manageNcmpcpp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnRanger  = myTerminal ++ " -t ranger -e ranger"
    findRanger   = title =? "ranger"
    manageRanger = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnBitwarden  = myTerminal ++ " -n bitwarden 'bitwarden'"
    findBitwarden   = resource =? "bitwarden"
    manageBitwarden = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-M1-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Windows
        , ("M-z", kill1)                             -- Kill the currently focused client
        , ("M-S-z", killAll)                         -- Kill all the windows on current workspace

    -- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                       -- Push ALL floating windows back to tile.
        , ("M-f", sendMessage (T.Toggle "floats"))        -- Toggle floating window

    -- Windows navigation
        , ("M-m", windows W.focusMaster)             -- Move focus to the master window
        , ("M-j", windows W.focusDown)               -- Move focus to the next window
        , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
        , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
        , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
        , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
        , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
        , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
        , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
        , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
        , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
        , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
        , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
        , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
        , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                              -- Switch to next layout
        , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
        , ("M-S-n", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
        , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-u", sendMessage $ Toggle MIRROR)
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-C-j", sendMessage MirrorShrink)
        , ("M-C-k", sendMessage MirrorExpand)

    -- Workspaces
        -- mod-{1-9}, Move to workplace, these work automatically
        -- mod-shift {1-9}. Move client to workplace, these work automatically

    -- Physical Screens
        -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3, these work automatically
        -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3, these work automatically
        , ("M-,", nextScreen)                           -- Switch focus to next monitor
        , ("M-.", prevScreen)                           -- Switch focus to prev monitor
        , ("M-S-,", shiftNextScreen)                    -- Move window to next monitor
        , ("M-S-.",  shiftPrevScreen)                   -- Move window to prev monitor


    -- Open My Preferred Terminal.
        , ("M-<Return>", spawn (myTerminal ++ " -e bash"))

    --- Rofi and Dmenu Scripts
        , ("M-S-<Return>", spawn "rofi -modi drun -show drun -show-icons")
        , ("M-M1-v", spawn "~/bin/rofi-locate")
        , ("M-M1-n", spawn "networkmanager_dmenu")
        , ("M-M1-c", spawn "rofi -show emoji -modi emoji")
        , ("M-M1-m", spawn "rofi -modi file-browser -show file-browser")
        , ("M-x", spawn "~/bin/powerspec")
        , ("M-M1-r", spawn "dmenufm")
       
    --- My Applications (Super+Alt+Key)
	    , ("M-M1-t", spawn "telegram-desktop")
	    , ("M-M1-p", spawn "pcmanfm")
	    , ("M-M1-g", spawn "google-chrome-stable")
	    , ("M-M1-f", spawn "firefox")
	    , ("M-M1-b", spawn "/usr/bin/bitwarden")

    --- My Scratchpads
        , ("C-1", namedScratchpadAction myScratchPads "terminal")
        , ("C-2", namedScratchpadAction myScratchPads "ncmpcpp")
        , ("C-3", namedScratchpadAction myScratchPads "ranger")
        , ("C-4", namedScratchpadAction myScratchPads "bitwarden")
       
    -- Multimedia Keys
	    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("M-M1-<F8>", spawn "scrot 0")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
                
------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape) 
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where                                                                      
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,                                        
                      let n = i ] 

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [  className =? "Pcmanfm"  --> doShift ( myWorkspaces !!1 )
      , className =? "Google-chrome"     --> doShift ( myWorkspaces !! 2 )
      , className =? "Firefox"     --> doShift ( myWorkspaces !! 2 )
      , className =? "TelegramDesktop" --> doShift ( myWorkspaces !! 8 )
      , className =? "Gcolor3"  --> doFloat
      , className =? "vlc"         --> doShift "<action=xdotool key super+8>vid</action>"
      , title =? "Oracle VM VirtualBox Manager"  --> doFloat
      , className =? "Oracle VM VirtualBox Manager"  --> doShift "<action=xdotool key super+5>vbox</action>"
      , className =? "Gimp"        --> doFloat
      , className =? "Gimp"        --> doShift "<action=xdotool key super+9>gimp</action>"
      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
      ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ 
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where 
                 myDefaultLayout = tall ||| noBorders monocle ||| grid ||| threeRow ||| oneBig ||| space ||| floats


tall       = renamed [Replace "tall"]     $ limitWindows 12 $ mySpacing 6 $ ResizableTall 1 (3/100) (1/2) []
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 $ Full
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ mySpacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeRow   = renamed [Replace "threeRow"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "oneBig"]   $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
space      = renamed [Replace "space"]    $ limitWindows 4  $ mySpacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat

