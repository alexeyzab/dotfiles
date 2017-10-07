import           Data.List                    (isSuffixOf)
import qualified Data.Map                     as M
import           System.IO
import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.Maximize
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

myWorkspaces = ["1:terminal","2:web","3","4","5","6","7","8"]
myManageHook =
  composeAll
    [ className =? "google-chrome-stable" --> doShift "2:web"
    , className =? "urxvtc" --> doShift "1:terminal"
    , className =? "Gimp-2.8" --> doShift "*"
    , (className =? "Gimp-2.8" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

main :: IO ()
main = do
    xmproc <- spawnPipe "/home/alexeyzab/.local/bin/xmobar /home/alexeyzab/.config/xmobar/xmobarrc"

    xmonad $ ewmh def
      { manageHook = myManageHook <+> manageDocks
                                  <+> manageHook def
                                  <+> namedScratchpadManageHook scratchpads
      , handleEventHook = docksEventHook <+> handleEventHook def
      , layoutHook = maximize $ avoidStruts $ layoutHook def
      , logHook = updatePointer (0.5, 0.5) (0, 0) >>
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppCurrent = xmobarColor "#bfbfbf" ""
          , ppTitle = xmobarColor "#2d2d2d" "" . shorten 50
          , ppLayout = const ""
          }
      , modMask = mod4Mask
      , terminal = "urxvtc"
      , workspaces = myWorkspaces
      , normalBorderColor = "#000000"
      , focusedBorderColor = "#839496"
      } `additionalKeysP`
      [ ("M-p", spawn "rofi -show run -m -4 -font 'fantasque sans mono 12'")
      , ("M-S-l", spawn "slock")
      , ("M-b", sendMessage ToggleStruts)
      , ("M-s", namedScratchpadAction scratchpads "scratch")
      , ("M-u", namedScratchpadAction scratchpads "mail")
      , ("M-z", namedScratchpadAction scratchpads "zeal")
      -- , ("<XF86AudioRaiseVolume>", spawn "amixer sset IEC958 5%+")
      -- , ("<XF86AudioLowerVolume>", spawn "amixer sset IEC958 5%-")
      -- , ("<XF86AudioMute>", spawn "amixer sset Master 1+ toggle")
      -- , ("M-f", spawn "pactl set-sink-volume 1 +5%")
      -- , ("M-d", spawn "pactl set-sink-volume 1 -5%")
      -- , ("M-c", spawn "pactl set-sink-mute 1 toggle")
      -- , ("M-i", spawn "light -s mba6x_backlight -U 10")
      -- , ("M-o", spawn "light -s mba6x_backlight -A 10")
      , ("M-y", withFocused (sendMessage . maximizeRestore))
      ]

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "mail" "urxvtc -e mutt" (title =? "mutt")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    , NS "scratch" "urxvtc --title scratch" (title =? "scratch")
        -- (customFloating $ W.RationalRect 0.6 0.8 0.35 0.1)
        (customFloating $ W.RationalRect 0.3 0.5 0.4 0.2)
    , NS "zeal" "zeal" (className =? "Zeal")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    ]
