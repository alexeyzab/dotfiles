#!/bin/sh

essid=`iwlist wlp3s0 scan | grep "ESSID" | cut -d':' -f2 | sed 's/"//g'`
strngth=`iwlist wlp3s0 scan | grep "Signal level" | cut -d'=' -f3 | cut -c2,3`
bars=`expr $strngth / 10`

case $bars in
  0)  bar='[----------]' ;;
  1)  bar='[/---------]' ;;
  2)  bar='[//--------]' ;;
  3)  bar='[///-------]' ;;
  4)  bar='[////------]' ;;
  5)  bar='[/////-----]' ;;
  6)  bar='[//////----]' ;;
  7)  bar='[///////---]' ;;
  8)  bar='[////////--]' ;;
  9)  bar='[/////////-]' ;;
  10) bar='[//////////]' ;;
  *)  bar='[----!!----]' ;;
esac

echo $essid $bar

exit 0
