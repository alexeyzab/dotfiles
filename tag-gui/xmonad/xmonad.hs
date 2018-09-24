import           Data.List                    (isSuffixOf)
import qualified Data.Map                     as M
import           System.IO
import           Codec.Binary.UTF8.String     as UTF8
import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           System.Taffybar.Support.PagerHints (pagerHints)


main :: IO ()
main = do
  xmonad $ docks $ ewmh $ pagerHints $ myConfig

myConfig = def
      { manageHook = myManageHook <+> manageHook def <+> manageDocks
                                  <+> namedScratchpadManageHook scratchpads
      , handleEventHook = docksEventHook <+> handleEventHook def
      , layoutHook = avoidStruts $ smartBorders $ layoutHook def
      , logHook = dynamicLog >> updatePointer (0.25, 0.25) (0.25, 0.25)
      , startupHook = myStartupHook
      , modMask = mod4Mask
      , terminal = "alacritty"
      , workspaces = myWorkspaces
      , normalBorderColor = "#000000"
      , focusedBorderColor = "#839496"
      } `additionalKeysP`
      [ ("M-p", spawn "rofi -show run -m -1 -font 'fantasque sans mono 12'")
      , ("M-S-p", spawn "rofi -show window -m -1 -font 'fantasque sans mono 12'")
      , ("M-o", spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'")
      , ("M-S-l", spawn "slock")
      , ("M-S-s", spawn "screenshot")
      , ("M-S-u", spawn "rofi-pass")
      , ("M-b", sendMessage ToggleStruts)
      , ("M-s", namedScratchpadAction scratchpads "scratch")
      , ("M-u", namedScratchpadAction scratchpads "mail")
      , ("M-z", namedScratchpadAction scratchpads "zeal")
      , ("M-f", spawn "amixer -q set Master 5%+")
      , ("M-d", spawn "amixer -q set Master 5%-")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
      , ("<XF86AudioMute>", spawn "amixer -q set Master 1+ toggle")
      , ("M-y", withFocused (sendMessage . maximizeRestore))
      ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myManageHook =
  composeAll
    [ className =? "Google-chrome" --> doShift "2"
    , className =? "Firefox" --> doShift "2"
    , className =? "Alacritty" --> doShift "1"
    , className =? "Emacs" --> doShift "1"
    , className =? "Slack" --> doShift "3"
    , className =? "Gimp-2.8" --> doShift "*"
    , (className =? "Gimp-2.8" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

-- Fix for Intellij
myStartupHook = do
  setWMName "LG3D"

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "mail" "alacritty -e mutt" (title =? "mutt")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    , NS "scratch" "alacritty --title scratch" (title =? "scratch")
        (customFloating $ W.RationalRect 0.3 0.5 0.4 0.2)
    , NS "zeal" "zeal" (className =? "Zeal")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    ]
