import           Data.List                    (isSuffixOf)
import qualified Data.Map                     as M
import           System.IO
import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run


main :: IO ()
main = do
    xmonad =<< statusBar "" defaultPP toggleStrutsKey (ewmh $ docks $ myConfig)

myConfig = def
      { manageHook = myManageHook <+> manageHook def <+> manageDocks
                                  <+> namedScratchpadManageHook scratchpads
      , handleEventHook = docksEventHook <+> handleEventHook def
      , layoutHook = avoidStrutsOn [U] $ smartBorders $ layoutHook def
      , modMask = mod4Mask
      , terminal = "urxvtc"
      , workspaces = myWorkspaces
      , normalBorderColor = "#000000"
      , focusedBorderColor = "#839496"
      } `additionalKeysP`
      [ ("M-p", spawn "rofi -show run -m -4 -font 'fantasque sans mono 12'")
      , ("M-t", spawn "rofi -show window -m -4 -font 'fantasque sans mono 12'")
      , ("M-S-l", spawn "~/.local/bin/better-slock")
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
    [ className =? "chromium-browser" --> doShift "2"
    , className =? "urxvtc" --> doShift "1"
    , className =? "Gimp-2.8" --> doShift "*"
    , (className =? "Gimp-2.8" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "mail" "urxvtc -e mutt" (title =? "mutt")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    , NS "scratch" "urxvtc --title scratch" (title =? "scratch")
        (customFloating $ W.RationalRect 0.3 0.5 0.4 0.2)
    , NS "zeal" "zeal" (className =? "Zeal")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    ]
