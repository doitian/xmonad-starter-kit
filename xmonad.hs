{-# OPTIONS_GHC -fcontext-stack=32 #-}
{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import Control.OldException(catchDyn,try)

import Data.Char (toLower)
import Data.Monoid (mappend)
import Data.List (intercalate, intersperse, isSuffixOf, isPrefixOf)
import qualified Data.Map as M (fromList)

import System.Exit (exitSuccess)
import System.Posix (sleep)

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (findWorkspace, nextScreen, prevScreen, swapNextScreen, swapPrevScreen, toggleOrDoSkip, WSType(..))
-- import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.Promote
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
import qualified XMonad.Actions.FlexibleResize as Flex

import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutModifier ( ModifiedLayout(..) )

import XMonad.Layout.GridVariants ( Grid(..) )
import XMonad.Layout.IM
import XMonad.Layout.OneBig
import XMonad.Layout.MultiColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Window
import XMonad.Prompt.Man

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (WorkspaceCompare, WorkspaceSort, mkWsSort, getWsIndex)

getWsCompare' :: X WorkspaceCompare
getWsCompare' = do
    wsIndex <- getWsIndex
    return $ \a b -> f (wsIndex a) (wsIndex b) `mappend` compare a b
  where
    f Nothing Nothing   = EQ
    f (Just _) Nothing  = LT
    f Nothing (Just _)  = GT
    f (Just x) (Just y) = compare x y

getSortByIndex' :: X WorkspaceSort
getSortByIndex' = mkWsSort getWsCompare'

-- Dependencids
-- xdotool, wmctrl

myTerminal      = "urxvt"
-- window class, use `M-c i` to inspect the window class name
myTerminalClass = "URxvt"
myBorderWidth   = 2
myModMask       = mod4Mask

myFont = "WenQuanYi Micro Hei Mono"
myMonoFont = "WenQuanYi Micro Hei Mono"

myNormalBorderColor  = "#444444"
myFocusedBorderColor = "#268bd2"
myActiveBorderColor  = "#444444"

-- shell prompt theme
mySP = defaultXPConfig
       { font = "xft:" ++ myMonoFont ++ ":pixelsize=14"
       , bgColor           = "#002b36"
       , fgColor           = "#93a1a1"
       , bgHLight          = "#93a1a1"
       , fgHLight          = "#002b36"
       , borderColor       = "#2aa198"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 22
       , defaultText       = []
       }

myAutoSP = mySP { autoComplete       = Just 1000 }
myWaitSP = mySP { autoComplete       = Just 1000000 }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        c <- withDisplay $ \d -> fmap resClass $ io $ getClassHint d w
        whenJust (W.findTag w ws) (flash name c)
      where flash _ "Pidgin" _ = spawn "true"
            flash _ "emesene" _ = spawn "true"
            flash name c index = spawn $
                                 intercalate " " $
                                 [ "notify-send -i"
                                 , icon
                                 , show $ show name
                                 , show $ "on " ++ index ]
                where icon = case c of
                               "URxvt" -> "gnome-terminal"
                               otherwise -> map toLower c

focusUrgent' :: X ()
focusUrgent' = withUrgents $ (windows . focusOrSwitch)
               where
                 focusOrSwitch (w:_) = W.focusWindow w
                 focusOrSwitch _   = W.greedyView =<< W.tag . head . namedScratchpadFilterOutWorkspace . W.hidden

myDzenPP h = defaultPP
             { ppOutput   = hPutStrLn h
             , ppCurrent  = dzenColor "#002b36" myFocusedBorderColor . pad
             , ppVisible  = dzenColor myFocusedBorderColor "#073642" . pad . dzenSwitchWs
             , ppUrgent   = dzenColor "#dc322f" "" . pad . dzenSwitchWs
             , ppHidden   = pad . dzenSwitchWs
             , ppLayout   = dzenColor "#b58900" "" . wrap " ^ca(1,xdotool key Super_L+backslash)[" "]^ca() "
             , ppTitle    = dzenColor "#93a1a1" "" . dzenEscape
             , ppSep      = ""
             , ppWsSep    = "^fg(#073642)^r(2x16)^fg()"
             , ppSort     = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex'
             , ppExtras   = [ppScreens]
            }
               where ppScreens = do ws <- gets windowset
                                    let cv = [W.current ws] ++ W.visible ws
                                        tags = map (\w -> (show $ fromIntegral $ W.screen w) ++ "." ++ (W.tag $ W.workspace w)) cv
                                    return $ Just ("\0000" ++ concat (intersperse "\0000" tags))

dzenSwitchWs :: String -> String
dzenSwitchWs s = "^ca(1,switch-workspace.zsh " ++ (show s) ++ ")" ++ s ++ "^ca()"

myTheme :: Theme
myTheme = defaultTheme
          { fontName = "xft:" ++ myFont ++ ":pixelsize=14"
          , decoHeight = 20
          , decoWidth = 400
          , activeColor = myFocusedBorderColor
          , inactiveColor = "#262626"
          , urgentColor = "#073642"
          , activeBorderColor = myFocusedBorderColor
          , inactiveBorderColor = "#586e75"
          , urgentBorderColor = "#586e75"
          , activeTextColor = "#002b36"
          , inactiveTextColor = "#839496"
          , urgentTextColor = "#dc322f"
          }

pads = [ NS "term"
         (myTerminal ++ " -name scratchpad")
         (resource =? "scratchpad" <&&> className =? myTerminalClass)
         (customFloating $ W.RationalRect 0.2 0.6 0.6 0.4)
       ]

-- unused char
-- a, x, y, ', m
myKeys =  \conf -> mkKeymap conf $
    [ ("M-S-<Return>", spawn $ XMonad.terminal conf) -- terminal
    , ("M-`", namedScratchpadAction pads "term") -- quake terminal

    -- prompt
    , ("M-p g", windowPromptGoto myWaitSP) -- window go prompt
    , ("M-p b", windowPromptBring myWaitSP) -- window bring prompt
    , ("M-p d", AL.launchApp mySP { defaultText = "~" } "pcmanfm") -- directory prompt
    , ("M-p o", AL.launchApp mySP "xopen" ) -- open prompt
    , ("M-p m", manPrompt mySP ) -- xmonad prompt
    , ("M-g", runOrRaise "window-go.sh" (resource =? "WindowGo" <&&> className =? "Gpicker")) -- window go
    , ("M-b", runOrRaise "window-bring.sh" (resource =? "WindowBring" <&&> className =? "Gpicker")) -- window bring
    , ("M-<Return>", raiseMaybe (spawn "gmrun") (className =? "Gmrun")) -- gmrun

    , ("M-S-s", spawn "x-www-browser \"http://www.google.com/search?q=`xclip -o`\"") -- search selection
    , ("M-S-o", spawn "xopen -") -- open current selection

    -- app
    , ("M-o", runOrRaiseNext "firefox" (className =? "Firefox" <||> className =? "Google-chrome" <||> className =? "Chromium")) -- browser
    , ("M-i", runOrRaiseNext "emacs-dwim" (className =? "Emacs")) --emacs
    , ("M-u", runOrRaiseNext myTerminal (className =? myTerminalClass <&&> resource /=? "scratchpad")) -- raise next terminal

    , ("M-c t", raiseNextMaybe (spawn $ myTerminal ++ " -name htop -e htop") (resource =? "htop")) -- Top
    , ("M-c r", raiseNextMaybe (spawn "pcmanfm") (resource =? "pcmanfm")) -- File Browser
    , ("M-c h", spawn "xmonad-key.sh") -- Help
    , ("M-c i", spawn "xp") -- Window Info
    , ("M-c x", spawn "xkill") -- Kill X app
    , ("M-c d", spawn "dropbox stop && dropbox start") -- Restart Dropbox
    , ("M-c S-d", spawn "notify-send -i dropbox `dropbox status`") -- Dropbox Status
    , ("M-c c", runOrRaiseNext "gsimplecal" (className =? "Gsimplecal")) --gsimplecal

    -- client
    , ("M-S-c", kill1) -- kill
    , ("M-C-c", kill) -- kill all
    , ("M-C-S-c", kill) -- kill all
    , ("M-S-<Backspace>", kill1) -- kill
    , ("M-S-<Backspace>", kill1) -- kill
    , ("M-c M-S-c", killAll) -- kill
    , ("M-S-=", windows copyToAll) -- copy to all
    , ("M-=",  killAllOtherCopies) -- kill others
    , ("M-C-S-=",  kill) -- kill all

    -- make sure mod matches keysym
    , ("M-a", rotSlavesUp) -- rotate slaves up
    , ("M-S-a", rotSlavesDown) -- rotate slaves down
    , ("M-<Tab>", windows W.focusDown) -- focus down
    , ("M-S-<Tab>", windows W.focusUp) -- focus up
    , ("M-;", windows W.focusMaster) -- focus master
    , ("M-S-;", promote) -- promote to master
    , ("M-z", focusUrgent) -- focus urgent
    , ("M-S-<Page_Down>", windows W.swapDown  ) -- swap down
    , ("M-S-<Page_Up>", windows W.swapUp    ) -- swap up

    , ("M-[", sendMessage Shrink) -- shrink master
    , ("M-]", sendMessage Expand) -- expand master

    , ("M-h", sendMessage $ Go L) -- focus left
    , ("M-j", sendMessage $ Go D) -- focus down
    , ("M-k", sendMessage $ Go U) -- focus up
    , ("M-l", sendMessage $ Go R) -- focus right
    , ("M-S-h", sendMessage $ Swap L) -- swap left
    , ("M-S-j", sendMessage $ Swap D) -- swap down
    , ("M-S-k", sendMessage $ Swap U) -- swap up
    , ("M-S-l", sendMessage $ Swap R) -- swap right
    , ("M-C-h", sendMessage $ Move L) -- move left
    , ("M-C-j", sendMessage $ Move D) -- move down
    , ("M-C-k", sendMessage $ Move U) -- move up
    , ("M-C-l", sendMessage $ Move R) -- move right

    -- float
    , ("M-<L>", withFocused (keysMoveWindow (-20,0))) -- move float left
    , ("M-<R>", withFocused (keysMoveWindow (20,0))) -- move float right
    , ("M-<U>", withFocused (keysMoveWindow (0,-20))) -- move float up
    , ("M-<D>", withFocused (keysMoveWindow (0,20))) -- move float down
    , ("M-S-<L>", withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top

    -- layout
    , ("M-\\", sendMessage NextLayout) -- toggle layouts
    , ("M-S-\\", myLayoutPrompt) -- layout prompt
    , ("M-C-\\", setLayout $ XMonad.layoutHook conf) -- reset layout
    , ("M-f", sendMessage (Toggle FULL)) -- toggle Full
    , ("M-s", sendMessage (Toggle SIDEBAR)) -- toggle sidebar
    , ("M-d", sendMessage (Toggle MAG)) -- toggle mag
    , ("M-<Escape>", sendMessage (Toggle RFULL)) -- Full without panel, border
    , ("M-t", withFocused $ windows . W.sink) -- sink focused window
    , ("M-S-t", sinkAll) -- sink all windows
    , ("M-C-[", sendMessage (IncMasterN (-1))) -- decrease master windows number
    , ("M-C-]", sendMessage (IncMasterN 1)) -- increase master windows number

    -- system
    , ("M-C-S-q", io (exitHook >> exitSuccess)) -- exit
    , ("M-S-q",  broadcastMessage ReleaseResources >> io exitHook >> restart "xmonad" True) -- restart
    , ("M-c M-C-S-q", io $ spawn "gksu /sbin/poweroff") -- poweroff
    , ("M-c M-C-S-r", io $ spawn "gksu /sbin/reboot") -- reboot
    , ("M-q", refresh) -- refresh

    -- cycle through workspaces
    , ("M-n", withWorkspace' (windows . W.greedyView)) -- workspace prompt gready
    , ("M-m", withWorkspace' (windows . W.view)) -- workspace prompt
    , ("M-S-n", withWorkspace' (windows . W.shift)) -- workspace shift prompt
    , ("M-C-n", withWorkspace' (windows . copy)) -- workspace copy prompt
    , ("M-C-S-n", renameWorkspace mySP) -- rename workspace
    , ("M-C-S-<Backspace>", removeWorkspace) -- delete empty workspace
    , ("M-/", toggleWSNoSP) -- toggle recently visited workspaces
    ]
    ++
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
        , (f, m) <- [ (W.greedyView, "")
                    , (W.shift, "S-")
                    , (copy, "C-")
                    ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ [k], (windows $ W.shift i) >> (windows $ W.greedyView i))
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
    ]
    ++
    -- "M-{w,e,r}" -- Switch to physical/Xinerama screens 1, 2, or 3
    -- "M-S-{w,e,r}" -- Move client to screen 1, 2, or 3
    --
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]
    ++
    [ ("M-'", prevScreen) -- Prev Screen
    , ("M-S-'", swapNextScreen) -- Swap next screen
    ]
    ++
    -- HiddenNonEmptyWS
    [ ("M-.", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- go to next workspace
    , ("M-,", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- go to prev workspace
    , ("M-S-.", windows . W.shift =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- shift to next workspace
    , ("M-S-,", windows . W.shift =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- shift to prev workspace
    -- move window to and focus HiddenNonEmpty wss except scratchpad
    , ("M-C-S-.", shiftAndView' Next) -- shift to next workspace and follow
    , ("M-C-S-,", shiftAndView' Prev) -- shift to prev workspace and follow
    ]
    where
      getSortByIndexNoSP =
          fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex'
      shiftAndView' dir = findWorkspace getSortByIndexNoSP dir AnyWS 1
                          >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
      toggleWSNoSP = windows $ W.greedyView =<< W.tag . head . namedScratchpadFilterOutWorkspace . W.hidden
      role = stringProperty "WM_WINDOW_ROLE"

dmenuXinerama :: [String] -> X String
dmenuXinerama opts = do
    io $ runProcessWithInput "dmenu" [ "-fn"
                                     , (myFont ++ "-10")
                                     , "-nb"
                                     , "#1e2320" 
                                     , "-nf" 
                                     , "#acbc90" 
                                     , "-sf" 
                                     , "#0f1a0f"
                                     , "-sb"
                                     , "#f0dfaf"
                                     ] (unlines opts)

withWorkspace' :: (String -> X ()) -> X ()
withWorkspace' job = do sort <- fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex'
                        ws <- gets (map W.tag . sort . W.workspaces . windowset)
                        w <- dmenuXinerama ws
                        let w' = case reverse w of
                              '\n':xs -> reverse xs
                              otherwise -> w
                          in case w' of (_:_)
                                         | w' `elem` ws -> job w'
                                         | otherwise -> addHiddenWorkspace w' >> job w'

myLayoutPrompt :: X ()
myLayoutPrompt = do l <- dmenuXinerama layouts
                    sendMessage $ JumpToLayout $ drop 2 l
                      where
                        layouts = ["1.cols", "2.two", "3.rows", "4.tab", "5.grid", "6.big"]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    -- forward,backward button8 and button9
    , ((0, 8), (\w -> windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1)) -- go to next workspace
    , ((0, 9), (\w -> windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1)) -- go to prev workspace
    ]
    where
      getSortByIndexNoSP =
          fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex'

------------------------------------------------------------------------
-- Layouts:

data MyTransformers = SIDEBAR 
                    | MAG
                    | RFULL
  deriving (Read, Show, Eq, Typeable)
instance Transformer MyTransformers Window where
  transform SIDEBAR x k = k (withIM (1/5) (Const True) x) (\(ModifiedLayout _ x') -> x')
  transform MAG x k = k (Mag.magnifiercz 1.2 x) (\(ModifiedLayout _ x') -> x')
  transform RFULL x k = k (avoidStrutsOn [] $ noBorders Full) (const x)

myLayout = configurableNavigation (navigateColor myActiveBorderColor)
           $ mkToggle (single RFULL)
           $ avoidStruts
           $ mkToggle (single MAG)
           $ mkToggle (single FULL)
           $ (onWorkspace "8.gimp" $ named "gimp" $ withIM (2/11) (Role "gimp-toolbox") $ big')
           $ mkToggle (single SIDEBAR) 
           $ layouts
  where
    layouts  = cols' ||| twopane' ||| rows' ||| tabs' ||| grid' ||| big'
    cols'    = named "cols" $ layoutHints $ deco $ multiCol [1] 2 (3/100) (1/2)
    twopane' = named "two"  $ layoutHints $ TwoPane (3/100) (3/7)
    rows'    = named "rows" $ Mirror $ layoutHints $ deco $ multiCol [2] 3 (2/100) (4/7)
    tabs'    = named "tab"  $ layoutHints $ tabs
    grid'    = named "grid" $ layoutHints $ deco $ Grid (16/10)
    big'     = named "big"  $ layoutHints $ deco $ Mirror $ OneBig (3/4) (19/24)
    -- basic layouts
    tabs     = tabbed shrinkText myTheme
    deco     = dwmStyle shrinkText myTheme
    -- property query
    role = stringProperty "WM_WINDOW_ROLE"

myWorkspaces    = ["1.sys","2.www","3.editor","4.doc","5","6","7","8.gimp","9.im","0","-"]

myFloatManageHook = composeOne . concat $
    [ [ (className =? "Gsimplecal" -?> doRectFloat (W.RationalRect 0.75 0.02 0.25 0.23))
      , (className =? "Screenkey" -?> doIgnore)
      , (role =? "gimp-toolbox" -?> doMaster)
      ]
    , [ className =? x -?> doCenterFloat' | x <- cCenter ]
    , [ className =? x -?> doMaster | x <- masters ]
    ]
    where
      cCenter = [ "Gmrun", "Gpicker", "Gcolor2" ]
      masters = [ "Emacs" ]
      doCenterFloat' = doCenterFloat <+> doMaster
      doMaster = doF W.shiftMaster
      role = stringProperty "WM_WINDOW_ROLE"
      
mySmartFloatManageHook = composeOne . concat $
  [ [ (isFullscreen -?> doFullFloat') ]
  , [ className =? x -?> doFloat' | x <- cFloat ]
  , [ role =? x -?> doFloat' | x <- rFloat ]
  , [ (isDialog -?> doFloat') ] ]
  where
    cFloat  = [ "Zenity", "Stardict", "Update-manager", "Shutter"]
    rFloat  = [ "gimp-dock" ]
    ffCenter = [ "Manager", "Extension", "Download", "Dialog", "Browser", "Toplevel" ]
    unFloat = ask >>= doF . W.sink
    doFullFloat' = doFullFloat <+> doMaster
    doFloat' = doFloat <+> doMaster
    doMaster = doF W.shiftMaster
    role = stringProperty "WM_WINDOW_ROLE"

myShiftManageHook = composeOne . concat $
    [ [ transience ]
    , [ className =? c -?> doShift t
        | (c, t) <- [ ("Pidgin", "9.im")
                    , ("Skype", "9.im")
                    , ("Gimp", "8.gimp")
                    , ("Emacs", "3.emacs")
                    ]
      ]
    , [ (resource =? "DTA" <&&> role =? "Manager" -?> doShift "0") ]
    ]
    where
      role = stringProperty "WM_WINDOW_ROLE"
myManageHook = insertPosition Below Newer
               <+> namedScratchpadManageHook pads
               <+> myShiftManageHook
               <+> myFloatManageHook
               <+> (placeHook $ inBounds $ withGaps (16,16,16,16) (smart (0.5, 0.5)))
               <+> mySmartFloatManageHook
               <+> manageDocks

myHandleEventHook = focusOnMouseMove

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = adjustEventInput
                >> setWMName "LG3D"

exitHook :: IO ()
exitHook = do
  return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  dzen <- spawnPipe $ "xmonad.panel -fn '" ++ myFont ++ "-10'"
  xmonad $ ewmh $ withUrgencyHook LibNotifyUrgencyHook defaultConfig
             { terminal           = myTerminal
             , focusFollowsMouse  = myFocusFollowsMouse
             , borderWidth        = myBorderWidth
             , modMask            = myModMask
             , workspaces         = myWorkspaces
             , normalBorderColor  = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor

             -- key bindings
             , keys               = myKeys
             , mouseBindings      = myMouseBindings

             -- hooks, layouts
             , layoutHook         = myLayout
             , manageHook         = myManageHook
             , handleEventHook    = myHandleEventHook
             , logHook            = (dynamicLogWithPP $ myDzenPP dzen)
                                    >> setWMName "LG3D"
             , startupHook        = myStartupHook
             }
             `additionalKeys`
             [ ((0, 0xff14), spawn "xscreensaver-command -lock || gnome-screensaver-command --lock")
             , ((mod4Mask, 0xff61), spawn "cd ~/Dump/Snapshots/ && scrot -e 'geeqie $f &'")
             , ((mod4Mask .|. controlMask, 0xff61), spawn "cd ~/Dump/Snapshots/ && sleep 0.2 && scrot -s -e 'geeqie $f &'")
             ]
