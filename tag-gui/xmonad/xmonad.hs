import XMonad
import XMonad.Layout.CenteredMaster
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO

myWorkspaces = ["1:terminal","2:web","3","4","5","6","7","8"]
myManageHook = composeAll
  [ className =? "chromium" --> doShift "2:web"
  , className =? "termite" --> doShift "1:terminal"
  ]

main :: IO ()
main = do
    xmproc <- spawnPipe "/home/alexeyzab/.cabal/bin/xmobar /home/alexeyzab/.config/xmobar/xmobarrc"

    xmonad $ defaultConfig
      { manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppCurrent = xmobarColor "#268BD2" ""
          , ppTitle = xmobarColor "#657B83" "" . shorten 50
          , ppLayout = const ""
          }
      , terminal = "st"
      , workspaces = myWorkspaces
      , normalBorderColor = "#000000"
      , focusedBorderColor = "#ffffff"
      } `additionalKeysP`
      [ ("M-p", spawn "gmrun")
      , ("M-S-l", spawn "slock")
      , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 5%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 5%-")
      , ("<XF86AudioMute>", spawn "amixer sset Master 1+ toggle")
      , ("M-f", spawn "amixer sset Master 5%+")
      , ("M-d", spawn "amixer sset Master 5%-")
      , ("M-t", spawn "amixer sset Master 1+ toggle")
      ]


