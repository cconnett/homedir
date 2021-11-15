{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

import Control.Monad
import Data.List
import Data.Monoid
import System.Environment
import System.Exit
import System.Process
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.Minimize
import XMonad.Config.Gnome
import XMonad.Core
import XMonad.Layout.BoringWindows as B
import XMonad.Layout.LayoutCombinators ((**//*), (**|*), (|||))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.StackSet
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Replace

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "gnome-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 1

myModMask = mod1Mask

-- Number of workspaces and linking of workspaces across two screens like Gnome.
myWorkspaces = map show [1 .. 18]

chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

pair = chunk 2

-- Border colors for unfocused and focused windows, respectively.
--
--myNormalBorderColor  = "#dddddd"
myNormalBorderColor = "#666666"

--myFocusedBorderColor = "#1e90ff"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- launch gmrun
  , ((modm .|. shiftMask, xK_p), spawn "gmrun")
  , ((modm .|. shiftMask, xK_i), spawn "inkscape")
  , ( (modm .|. shiftMask, xK_e)
    , spawn "/usr/local/google/home/cjc/bin/mathematica")
    -- launch shutter
  , ((mod4Mask, xK_s), spawn "shutter -s")
  , ((mod4Mask, xK_Up), spawn "amixer -D pulse sset Master 5%+")
  , ((mod4Mask, xK_Down), spawn "amixer -D pulse sset Master 5%-")
  , ((mod4Mask, xK_m), spawn "amixer -D pulse sset Master toggle")
    -- close focused window
  , ((modm .|. shiftMask, xK_c), kill)
    -- Rotate through the available layout algorithms
  , ((modm, xK_Insert), sendMessage (Toggle MIRROR))
  , ((modm, xK_F11), sendMessage (Toggle FULL))
  , ( (modm, xK_F12)
    , cycleThroughLayouts [description myTiled, description myGrid])
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_Insert), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
  , ((modm, xK_n), refresh)
    -- Move focus to the next window
  , ((modm, xK_j), B.focusDown)
    -- Move focus to the previous window
  , ((modm, xK_k), B.focusUp)
    -- Move focus to the next visible workspace
  , ((modm, xK_Tab), windows focusNextVisible)
    -- Move focus to the previous window
  , ((modm .|. shiftMask, xK_Tab), windows focusPrevVisible)
    -- Minimize the current window
  , ((modm, xK_m), withFocused minimizeWindow)
    -- Restore next minimized window
  , ((modm .|. shiftMask, xK_m), withLastMinimized maximizeWindow)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
  ] ++
  [ ( (mod1Mask, key)
    , do focusedScreen <- withWindowSet (return . screen . current)
        --focus to left
         screenWorkspace 1 >>= flip whenJust (windows . W.view)
        --set windows on focus to virtual desktop pair's left
         windows $ W.view (screenPair !! 1)
        --focus to right
         screenWorkspace 0 >>= flip whenJust (windows . W.view)
        --set windows on focused screen to virtual desktop pair's right
         windows $ W.view (screenPair !! 0)
        -- focus to original screen
         screenWorkspace focusedScreen >>= flip whenJust (windows . W.view))
  | (screenPair, key) <- zip (pair $ XMonad.workspaces conf) [xK_1 .. xK_9]
        --, (f, extraMod) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
    -- Physical/Xinerama screens
    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- ++
  []

-- Workspaces
focusNextVisible, focusPrevVisible :: WindowSet -> WindowSet
focusNextVisible ws =
  W.view (tag $ workspace $ head $ visible ws ++ [current ws]) ws

focusPrevVisible ws =
  W.view (tag $ workspace $ last $ current ws : visible ws) ws

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , (\w -> XMonad.focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> XMonad.focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , (\w -> XMonad.focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
data OnlyBordersOn a =
  OnlyBordersOn [String]
                [a]
  deriving (Read, Show)

instance LayoutModifier OnlyBordersOn Window where
  unhook (OnlyBordersOn _ allWindows) =
    asks (borderWidth . config) >>= setBorders allWindows
  redoLayout (OnlyBordersOn classes allWindows) _ _ windowRectanglePairs = do
    windowsWithBorders <- filterM predicate windows
    width <- asks (borderWidth . config)
    setBorders windowsWithBorders width
    setBorders (windows \\ windowsWithBorders) 0
    return (windowRectanglePairs, Just $ OnlyBordersOn classes windows)
    where
      predicate = (liftM (`elem` classes)) . runQuery className
      windows = map fst windowRectanglePairs

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw =
  withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

bordersOn classes = ModifiedLayout $ OnlyBordersOn classes []

myGrid = Grid

myThreeColumn = ThreeColMid 1 (3 / 100) (1 / 3)

myLayout =
  bordersOn ["Gnome-terminal", "Xclock"] $ {-"Emacs",-}
  B.boringAuto $
  minimize $
  mkToggle (FULL ?? MIRROR ?? EOT) $ myTiled ||| myThreeColumn ||| myGrid

--myTiled   = (Tall 0 1 0 **//* Tall 0 1 0) **|* (Tall 0 1 0)
--myTiled = Tall 1 (1/100) (1080/1920)
myTiled = Tall 1 (3 / 100) (50 / 100) -- Default to middle because it
                                  -- applies to both screens

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "pidgin" --> doFloat
    , className =? "inkscape" --> doFloat
    , stringProperty "WM_ICON_NAME" =? "the-unicoder" --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "bubble" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  replace
  xmonad myConfig

confirm :: String -> X () -> X ()
confirm prompt action = do
  answer <- dmenu [prompt]
  when (prompt == answer) action

choices :: [(String, X ())] -> X ()
choices alist = do
  answer <- dmenu $ map fst alist
  when ((not . null) answer) $ maybe (return ()) id $ lookup answer alist

lock, logout, reload :: X ()
lock = io $ spawn "xscreensaver-command -lock"

logout = io $ exitWith ExitSuccess

reload = io $ spawn "xmonad --restart"

myConfig =
  defaults `additionalKeysP`
  [ ("M-o", spawn "google-chrome") {-(className =? "Google-chrome")-}
  --, ("M-S-o", spawn "google-chrome") {-(className =? "Google-chrome")-}
  , ("<XF86WWW>", spawn "google-chrome")
  , ("M-S-s", spawn "blueman-manager")
  , ("M-e", spawn "emacs")
  , ("M4-e", spawn "emacs")
                             --,("M-n", spawn "nautilus ~")
  , ("M-`", lock)
  , ( "M-<Esc>"
    , choices
        [ ("Lock", lock)
        , ("Logout", confirm "Logout?" logout)
        , ("Reload", reload)
        ])
  , ("M-S-<Esc>", confirm "Reload?" reload)
  ] `removeKeysP`
  ["M-w", "M-<Space>", "M-n"]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults =
  defaultConfig
      -- simple stuff
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , XMonad.workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
      -- key bindings
    , keys = myKeys
    , mouseBindings = myMouseBindings
      -- hooks, layouts
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook
    , startupHook = myStartupHook
    }

data Grid a
  = Grid
  | GridRatio Double
  deriving (Read, Show)

defaultRatio :: Double
defaultRatio = 16 / 9

instance LayoutClass Grid a where
  pureLayout Grid r = pureLayout (GridRatio defaultRatio) r
  pureLayout (GridRatio d) r = arrange d r . integrate

arrange :: Double -> Rectangle -> [a] -> [(a, Rectangle)]
arrange aspectRatio (Rectangle rx ry rw rh) st = zip st rectangles
  where
    nwins = length st
    ncols = min nwins 3 --(nwins - 1) `div` 3 + 1
    mincs = max 1 $ nwins `div` ncols
    extrs = nwins - ncols * mincs
    chop :: Int -> Dimension -> [(Position, Dimension)]
    chop n m =
      ((0, m - k * fromIntegral (pred n)) :) .
      map (flip (,) k) . tail . reverse . take n . tail . iterate (subtract k') $
      m'
      where
        k :: Dimension
        k = m `div` fromIntegral n
        m' = fromIntegral m
        k' :: Position
        k' = fromIntegral k
    xcoords = chop ncols rw
    ycoords = chop mincs rh
    ycoords' = chop (succ mincs) rh
    (xbase, xext) = splitAt (ncols - extrs) xcoords
    rectangles = combine ycoords xbase ++ combine ycoords' xext
      where
        combine ys xs =
          [Rectangle (rx + x) (ry + y) w h | (x, w) <- xs, (y, h) <- ys]
