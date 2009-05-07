#!/bin/sh

### Init Vars
traycmd=`which stalonetray`
pid_file="$HOME/.xmonad/`basename $0`.pid"
e_cmdnoexec=2
e_alreadyrunning=3

### Error-Checking
if [ ! -x $traycmd ]; then
    echo "'$traycmd' is not accessible or executable" >&2
    exit $e_cmdnoexec
fi

### Pid Handling
if [ -f $pid_file ]; then
    pid=`cat $pid_file`
    if [ -d /proc/$pid ]; then
        echo "Already running an instance of traycmd.sh" >&2
        exit $e_alreadyrunning
    fi
    echo "Removing stale pid file"
    rm $pid_file
fi

### Run the command
$traycmd &
echo $! > $pid_file
wait

# vim:set ft=sh et ts=4 sts=4:
