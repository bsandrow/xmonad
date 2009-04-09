#!/bin/bash

TRAYER=`which trayer`

if [ -x "$TRAYER" ]; then
    exec $TRAYER --edge top\
                 --align right\
                 --SetDockType true\
                 --SetPartialStrut true\
                 --expand true\
                 --widthtype pixels\
                 --width 100\
                 --tint 0xFFFFFF\
                 --heighttype pixels\
                 --height 20
else
    echo "'trayer' not found or not executable."
    exit 1
fi
