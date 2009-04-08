x_startup_script=`which startup-x11.sh`
if [ -x $x_startup_script ]; then
    $x_startup_script
else
    urxvtd -o -q -f
fi


# Startup backgroud processes
xscreensaver &
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

# Reset settings for my Xmonad environment (in case they were muxed with if I
# log into another environment)
nautilus-show-desktop.sh off

# Startup graphical elements
xmonad-bottom-bar.sh &
[ -e $HOME/.fehbg ]; eval "`cat $HOME/.fehbg | egrep '^feh .*--bg-' | sed 's:^\\([^;|&]*\\).*$:\1:'`"

# Startup system tray applications
(
    stalonetray &
    sleep 1s
    update-notifier&
    nm-applet&
    gnome-power-manager&
) &

# finally, launch xmonad (with ssh-agent and dbus)
exec dbus-launch --exit-with-session ssh-agent xmonad
