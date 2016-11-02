import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.CenteredMaster
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run


myWorkspaces = ["1:terminal","2:web","3","4","5","6","7","8"]
myManageHook = composeAll
    [ className =? "Gimp-2.8" --> doShift "*"
    , (className =? "Gimp-2.8" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat
    , className =? "chromium" --> doShift "2:web"
    , className =? "termite" --> doShift "1:terminal"
    ]
  where role = stringProperty "WM_WINDOW_ROLE"

main :: IO ()
main = do
    xmproc <- spawnPipe "/home/alexeyzab/.cabal/bin/xmobar /home/alexeyzab/.config/xmobar/xmobarrc"

    xmonad $ def
      { manageHook = myManageHook <+> manageDocks
                                  <+> manageHook def
                                  <+> namedScratchpadManageHook scratchpads
      , handleEventHook = docksEventHook <+> handleEventHook def
      , layoutHook = avoidStruts $ layoutHook def
      , logHook = dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppCurrent = xmobarColor "#268BD2" ""
          , ppTitle = xmobarColor "#657B83" "" . shorten 50
          , ppLayout = const ""
          }
      , modMask = mod4Mask
      , terminal = "termite"
      , workspaces = myWorkspaces
      , normalBorderColor = "#000000"
      , focusedBorderColor = "#839496"
      } `additionalKeysP`
      [ ("M-p", spawn "gmrun")
      , ("M-S-l", spawn "slock")
      , ("M-b", sendMessage ToggleStruts)
      , ("M-s", namedScratchpadAction scratchpads "scratch")
      , ("M-u", namedScratchpadAction scratchpads "mail")
      , ("<XF86AudioRaiseVolume>", spawn "amixer sset IEC958 5%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer sset IEC958 5%-")
      , ("<XF86AudioMute>", spawn "amixer sset Master 1+ toggle")
      , ("M-f", spawn "amixer sset Master 5%+")
      , ("M-d", spawn "amixer sset Master 5%-")
      , ("M-m", spawn "amixer sset Master 1+ toggle")
      , ("M-i", spawn "xbacklight -dec 10")
      , ("M-o", spawn "xbacklight -inc 10")
      ]

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "mail" "termite -e mutt" (title =? "mutt")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    , NS "scratch" "termite --title scratch" (title =? "scratch")
        -- (customFloating $ W.RationalRect 0.6 0.8 0.35 0.1)
        (customFloating $ W.RationalRect 0.3 0.5 0.4 0.2)
    ]
