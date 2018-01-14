import System.Taffybar


import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.Battery
import System.Taffybar.SimpleClock
import System.Taffybar.NetMonitor

import System.Taffybar.Widgets.PollingGraph

import System.Taffybar.FreedesktopNotifications

import Graphics.UI.Gtk
import System.Information.Memory
import System.Information.CPU

textWidgetNew :: String -> IO Widget
textWidgetNew str = do
    box <- hBoxNew False 0
    label <- labelNew $ Just str
    boxPackStart box label PackNatural 0
    widgetShowAll box
    return $ toWidget box

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
    let clock = textClockNew Nothing "<span fgcolor='#bbbbbb'>%-I:%M %p, %D</span>" 1
        pager = taffyPagerNew defaultPagerConfig
            { widgetSep = " :: "
            , activeWorkspace = colorize "#ffaa00" "" . escape
            , visibleWorkspace = colorize "#aa5500" "" . escape
            --, emptyWorkspace = colorize "#444444" "" . escape
            }
        tray = systrayNew
        sep = textWidgetNew " ::"
        memCfg = defaultGraphConfig { graphDataColors = [ (1, 0.1, 0.1, 1) ]
                                    , graphBackgroundColor = (0.1, 0.1, 0.1)
                                    , graphDirection = RIGHT_TO_LEFT
                                    , graphPadding = 3
                                    , graphWidth = 50
                                    , graphBorderWidth = 0
                                    , graphLabel = Just "mem"
                                    }
        cpuCfg = defaultGraphConfig {
            graphDataColors = [ (1, 0.1, 0.1, 1) ]
          , graphBackgroundColor = (0.1, 0.1, 0.1)
          , graphDirection = RIGHT_TO_LEFT
          , graphPadding = 3
          , graphWidth = 50
          , graphBorderWidth = 0
          , graphLabel = Just "cpu"
        }
        mem = pollingGraphNew memCfg 1 memCallback
        cpu = pollingGraphNew cpuCfg 1 cpuCallback
        -- battery = batteryBarNew defaultBatteryConfig 10
        -- batteryTime = textBatteryNew "| $time$" 10
        --net = netMonitorNewWith 1k.5 "wlp3s0" 2 (formatNetworkInfo defaultNetFormat)
        -- net = netMonitorNew 2 "wlp3s0"
    defaultTaffybar defaultTaffybarConfig
                        { startWidgets = [ pager ]
                        , endWidgets = [ mem, cpu, clock, tray ]
                        }
