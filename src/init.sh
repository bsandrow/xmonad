#!/bin/sh

# urxvt daemon
urxvtd -o -q -f

# xscreensaver
xscreensaver &

# gpg agent
GPG_AGENT_INFO_FILE="$HOME/.gpg-agent-info"
if [ -f "$GPG_AGENT_INFO_FILE" ] && [ ! -r "$GPG_AGENT_INFO_FILE" ]; then
    echo "'$GPG_AGENT_INFO_FILE' exists but is unreadable. Aborting gpg-agent launch..."
elif [ -f "$GPG_AGENT_INFO_FILE" ] && kill -0 `cut -d: -f 2 $GPG_AGENT_INFO_FILE` 2>/dev/null; then
    GPG_AGENT_INFO=`cat $GPG_AGENT_INFO_FILE`
    export GPG_AGENT_INFO
else
    eval `gpg-agent --daemon`
    echo $GPG_AGENT_INFO >$GPG_AGENT_INFO_FILE
fi

# make sure nautilus doesn't want to draw the desktop if I launch it
nautilus-show-desktop.sh OFF

# background image
[ -e $HOME/.fehbg ]; eval "`cat $HOME/.fehbg | egrep '^feh .*--bg-' | sed 's:^\\([^;|&]*\\).*$:\1:'`"

# systray apps
(
    sleep 2s
    update-notifier&
    nm-applet&
    gnome-power-manager&
) &
