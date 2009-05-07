#!/bin/sh

######
###    init vars
######
xmonad_dir="$HOME/.xmonad"

######
###    background processes
######
urxvtd -o -q -f
xscreensaver &

######
###    gpg agent
######
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

######
###    settings
######
nautilus-show-desktop.sh OFF
[ -e $HOME/.fehbg ]; eval "`cat $HOME/.fehbg | egrep '^feh .*--bg-' | sed 's:^\\([^;|&]*\\).*$:\1:'`"

######
###    systray apps
######
(
    $xmonad_dir/traycmd.sh&
    sleep .5
    update-notifier&
    nm-applet&
    gnome-power-manager&
) &

######
###    info bar
######
$xmonad_dir/bottominfo.sh &

######
###    per host custom items
######
source "$xmonad_dir/`hostname`"
