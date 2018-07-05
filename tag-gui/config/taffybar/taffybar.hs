-- module Main where

-- import System.Taffybar.Context
-- import System.Taffybar.Widget.Systray
-- import System.Taffybar.Widget.TaffyPager
-- import System.Taffybar.Widget.Pager
-- import System.Taffybar.Widget.Battery
-- import System.Taffybar.Widget.SimpleClock
-- import System.Taffybar.Widget.NetMonitor

-- import System.Taffybar.Widgets.PollingGraph

-- import System.Taffybar.FreedesktopNotifications

-- import Graphics.UI.Gtk
-- import System.Information.Memory
-- import System.Information.CPU

-- textWidgetNew :: String -> IO Widget
-- textWidgetNew str = do
--     box <- hBoxNew False 0
--     label <- labelNew $ Just str
--     boxPackStart box label PackNatural 0
--     widgetShowAll box
--     return $ toWidget box

-- memCallback = do
--   mi <- parseMeminfo
--   return [memoryUsedRatio mi]

-- cpuCallback = do
--   (userLoad, systemLoad, totalLoad) <- cpuLoad
--   return [totalLoad, systemLoad]

-- main = do
--     let clock = textClockNew Nothing "<span fgcolor='#dfdfdf'>%-I:%M %p, %D</span>" 1
--         pager = taffyPagerNew defaultPagerConfig
--             { widgetSep = " :: "
--             , activeWorkspace = colorize "#ffaa00" "" . escape
--             , visibleWorkspace = colorize "#aa5500" "" . escape
--             --, emptyWorkspace = colorize "#444444" "" . escape
--             }
--         -- note = notifyAreaNew defaultNotificationConfig
--         tray = systrayNew
--         sep = textWidgetNew " ::"
--         memCfg = defaultGraphConfig { graphDataColors = [ (1, 0.1, 0.1, 1) ]
--                                     , graphBackgroundColor = (0.1, 0.1, 0.1)
--                                     , graphDirection = RIGHT_TO_LEFT
--                                     , graphPadding = 3
--                                     , graphWidth = 50
--                                     , graphBorderWidth = 0
--                                     , graphLabel = Just "mem"
--                                     }
--         cpuCfg = defaultGraphConfig {
--             graphDataColors = [ (1, 0.1, 0.1, 1) ]
--           , graphBackgroundColor = (0.1, 0.1, 0.1)
--           , graphDirection = RIGHT_TO_LEFT
--           , graphPadding = 3
--           , graphWidth = 50
--           , graphBorderWidth = 0
--           , graphLabel = Just "cpu"
--         }
--         mem = pollingGraphNew memCfg 1 memCallback
--         cpu = pollingGraphNew cpuCfg 1 cpuCallback
--         -- battery = batteryBarNew defaultBatteryConfig 10
--         -- batteryTime = textBatteryNew "| $time$" 10
--         --net = netMonitorNewWith 1k.5 "wlp3s0" 2 (formatNetworkInfo defaultNetFormat)
--         -- net = netMonitorNew 2 "wlp3s0"
--     defaultTaffybar defaultTaffybarConfig
--                         { startWidgets = [ pager ]
--                         , endWidgets = [ mem, cpu, clock, tray ]
--                         }

-- module Main where

-- import           System.Taffybar
-- import           System.Taffybar.Information.CPU
-- import           System.Taffybar.Information.Memory
-- import           System.Taffybar.SimpleConfig
-- import           System.Taffybar.Widget
-- import           System.Taffybar.Widget.Workspaces
-- import           System.Taffybar.Widget.Generic.PollingGraph

-- memCallback = do
--   mi <- parseMeminfo
--   return [memoryUsedRatio mi]

-- cpuCallback = do
--   (userLoad, systemLoad, totalLoad) <- cpuLoad
--   return [totalLoad, systemLoad]

-- main = do
--   let memCfg = defaultGraphConfig
--                { graphDataColors = [(1, 0, 0, 1)]
--                , graphLabel = Just "mem"
--                }
--       cpuCfg = defaultGraphConfig
--                { graphDataColors =
--                    [ (0, 1, 0, 1)
--                    , (1, 0, 1, 0.5)
--                    ]
--                , graphLabel = Just "cpu"
--                }
--   let clock = textClockNew Nothing "<span fgcolor='#dfdfdf'>%-I:%M %p, %D</span>" 1
--       workspaces = workspacesNew defaultWorkspacesConfig
--       windows = windowsNew defaultWindowsConfig
--       layout = layoutNew defaultLayoutConfig
--       -- note = notifyAreaNew defaultNotificationConfig
--       mpris = mpris2New
--       mem = pollingGraphNew memCfg 1 memCallback
--       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
--       tray = sniTrayNew
--   simpleTaffybar defaultSimpleTaffyConfig
--                    { startWidgets = [ workspaces, layout, windows, note ]
--                    , endWidgets = [ tray, clock, mem, cpu, mpris ]
--                    }
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import System.Taffybar
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.CommandRunner
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces

transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 50
  , graphBackgroundColor = transparent
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net "
  }

memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just "mem "
  }

cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just "cpu "
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      batteryPercentage = commandRunnerNew 5 "battery" [] "n/a"
      clock = textClockNew Nothing "<span fgcolor='#dfdfdf'>%-I:%M %p, %D</span>" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [ layout, windows ]
        , endWidgets = map (>>= buildContentsBox)
          [ batteryPercentage
          , batteryIconNew
          , cpu
          , mem
          , net
          , clock
          , mpris2New
          , sniTrayNew
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 35
        , widgetSpacing = 0
        }
  dyreTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
               toTaffyConfig myConfig
