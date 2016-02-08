{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}
-- Imports.
import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Actions.WindowGo (runOrRaiseAndDo)
import System.Posix.Types (ProcessID)
import XMonad.Hooks.EwmhDesktops(ewmh)
import System.Posix.Process (getProcessStatus)
import Control.Monad (liftM3, liftM2, liftM)
import TriPaneTall
import Data.IORef

-- The main function.
main :: IO ()
main = do
  led <- newIORef "led"
  ref <- newIORef initStat
  let ?ref = ref
  let ?led = led
  xcfg <- statusBar myBar myPP toggleStrutsKey myConfig
  mapM_ spawn myStartups
  xmonad $ ewmh xcfg

-- Command to launch the bar.
myBar :: String
myBar = "xmobar"

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myManageHook = composeAll
  [ className =? "Tilda" --> doFloat
  , isDialog --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "MPlayer" --> doFloat
  , className =? "Skype" --> doFloat
  , className =? "VirtualBox"  --> doFloat
  -- , className =? "Google-chrome" --> doFloat
  , manageDocks
  ]

myLayouts = avoidStruts (tiled' ||| Mirror tiled' ||| simplestFloat ||| myTabbed)
    ||| noBorders (fullscreenFull Full) ||| (maximize $ TriPaneTall 1 (1/3) 1 (1/2))
  where
    -- The default number of windows in the master pane
    nmaster  = 1
    -- Default proportion of screen occupied by master pane
    ratio    = 1/2
    -- Percent of screen to increment by when resizing panes
    delta    = 3/100
    -- Shorthand for ResizableTall
    tiled'   = maximize $ ResizableTall nmaster delta ratio []
    myTabbed = tabbed shrinkText myTabTheme


myConfig = defaultConfig { manageHook = myManageHook <+> manageHook defaultConfig
                         , modMask = mod4Mask
                         , layoutHook =  myLayouts
                         --avoidStruts $ layoutHook defaultConfig
                         , terminal = "lxterminal"
                         , focusedBorderColor = "#aaffff"
                         , focusFollowsMouse = False
                         , startupHook = setWMName "LG3D"
                         } `additionalKeys`
                         [ ((mod4Mask, xK_r), spawn "dmenu_run")
--                         , ((mod4Mask, xK_x), spawn "lxterminal" <+> doFloat)
                         , ((mod4Mask, xK_F4), kill)
--                         , ((mod4Mask, xK_d), spawnDropdownTerminal myConfig)
                         , ((mod4Mask, xK_c), spawn "google-chrome-unstable")
                         , ((mod4Mask, xK_t), spawn "lxterminal")
                         , ((mod4Mask, xK_f), spawn "pcmanfm")
                         , ((mod4Mask, xK_F12), spawn "xscreensaver-command -lock")
                         , ((mod4Mask, xK_Pause), toggleLED)
                         , ((0, xK_Print), spawn "scrot")
                         , ((mod1Mask, xK_Print), spawn "scrot -s")
                         , ((mod4Mask,  xK_u), withFocused $ windows . W.sink)
                         , ((mod4Mask, xK_m), withFocused (sendMessage . maximizeRestore))
                         ]


colorDarkGray, colorLightGray, colorWhite, colorLightBlue :: [Char]
colorDarkGray        = "#222222"
colorLightGray       = "#aaaaaa"
colorLightBlue       = "#0066ff"
colorWhite           = "#ffffff"

barXFont    :: [Char]
barXFont             = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*"

myTabTheme :: Theme
myTabTheme = defaultTheme { fontName = barXFont
                          , activeColor = colorLightBlue
                          , inactiveColor = colorDarkGray
                          , activeBorderColor = colorWhite
                          , inactiveBorderColor = colorLightBlue
                          , activeTextColor = colorWhite
                          , inactiveTextColor = colorLightGray
                          }

toggleLED :: (?led :: IORef String) => X ()
toggleLED = do
  led <- io $ readIORef ?led
  spawn $ "xset " ++ led ++ " 3"
  io $ writeIORef ?led (if (head led) == '-' then "led" else "-led")

data TERM = HIDDEN
          | SHOWN
          deriving (Show, Eq)

data TermStat = TermStat ProcessID TERM
              deriving (Show, Eq)

initStat :: TermStat
initStat = TermStat (-1) HIDDEN

pidq :: Query ProcessID
pidq = ask >>= (\w -> liftX $ withDisplay $ \d -> getPID d w)
    where getPID d w = getAtom "_NET_WM_PID" >>= \a -> io $
                       liftM getPID' (getWindowProperty32 d a w)
          getPID' (Just (x:_)) = fromIntegral x
          getPID' (Just [])     = -1
          getPID' (Nothing)     = -1

spawnDropdownTerminal :: (?ref :: IORef TermStat) => XConfig l -> X ()
spawnDropdownTerminal conf = do
  TermStat pid' stat <- io $ readIORef ?ref
  spawn $ "xmessage " ++ (show pid') ++ " " ++ (show stat)
  case stat of
    HIDDEN -> do
              runOrRaiseAndDo (terminal conf) (liftM2 (==) pidq $ return pid')
                             (\w -> withDisplay $
                                   \d -> do pid1 <- getPID d w
                                            io $ writeIORef ?ref (TermStat pid1 SHOWN)
                                            float w)
    SHOWN -> do
      runOrRaiseAndDo (terminal conf) (liftM2 (==) pidq $ return pid')
                          (\w -> withDisplay $
                                \d -> do pid1 <- getPID d w
                                         io $ writeIORef ?ref (TermStat pid1 HIDDEN)
                                         hide w)
    where getPID d w = getAtom "_NET_WM_PID" >>= \a -> io $
                       liftM getPID' (getWindowProperty32 d a w)
          getPID' (Just (x:_)) = fromIntegral x
          getPID' (Just [])     = -1
          getPID' (Nothing)     = -1

myStartups :: [String]
myStartups = ["/usr/libexec/polkit-gnome-authentication-agent-1",
              "nm-applet"]
