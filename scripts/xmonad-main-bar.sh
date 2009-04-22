#!/bin/sh
( sleep 1; stalonetray ) &
exec dzen2 -ta l -dock -bg "#333538" -h 20 -fg "#FFFFFF" -fn '-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*' -geometry -0+0
