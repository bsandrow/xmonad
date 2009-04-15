#!/bin/bash
#

### global settings
max_reset_interval=60

### email settings
email_interval=60
email_newmail_fgcolor='red'
email_fgcolor='green'

# dzen Settings
DZEN=`which dzen2`
FONT='-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*'
HEIGHT='14'
JUSTIFICATION='l'
FOREGROUND_COLOR="FFFFFF"
BACKGROUND_COLOR="111321"
BEHAVIOR='onstart=lower'
GEOMETRY='+0+754'

### clock settings
clock_fgcolor='yellow'
clock_interval=1

function get_mail_count()
{
    if [ -d "$1" ]; then
        echo -n `ls "$1" | wc -l`
    else
        echo -n '0'
    fi
}

function get_new_mail()
{
    maildir="$1"
    get_mail_count "$maildir/new"
}

function get_all_mail()
{
    maildir="$1"
    new_mail=`get_mail_count "$maildir/new"`
    cur_mail=`get_mail_count "$maildir/cur"`
    tmp_mail=`get_mail_count "$maildir/tmp"`
    echo $(($new_mail + $cur_mail + $tmp_mail))
}

function process_mailbox()
{
    ### options
    mailbox="$1"; label="$2"; alerts_toggle=$3
    ### mail counts
    new_mail=`get_new_mail "$mailbox"`
    all_mail=`get_all_mail "$mailbox"`
    ### output
    if [ $new_mail -ne 0 ] && [ $3 -eq 1 ]; then
        echo -n "[^fg($email_newmail_fgcolor)$label $new_mail/$all_mail^fg()] "
    else
        echo -n "[^fg($email_fgcolor)$label $new_mail/$all_mail^fg()] "
    fi
}

function print_email()
{
    process_mailbox "$HOME/mail/gmail/INBOX" "gmail" 1
    process_mailbox "$HOME/mail/rtk/INBOX" "rtk" 1
    process_mailbox "$HOME/mail/gmail/lists.xmonad" "xmonad" 0
    process_mailbox "$HOME/mail/gmail/lists.vim_use" "vim" 0
}


function print_clock()
{
    echo -n `date "+[ ^fg($clock_fgcolor)%a %b %d %Y^fg() | ^fg($clock_fgcolor)%k:%M (%Z)^fg() ]"`
}

count=0
while true
do
    # update each peice of information when the interval comes around, reset
    # the interval counter when we reach the max interval value
    [ $(( $count % $clock_interval ))     == 0 ] && clock_string=`print_clock`
    [ $(( $count % $email_interval ))     == 0 ] && email_string=`print_email`
    [ $(( $count % $max_reset_interval )) == 0 ] && count=0
    echo "$clock_string $email_string"
    count=$(( $count + 1 ))
    sleep 1
done | $DZEN -dock                          \
             -fg        "#$FOREGROUND_COLOR"\
             -bg        "#$BACKGROUND_COLOR"\
             -geometry  "$GEOMETRY"         \
             -fn        "$FONT"             \
             -ta        $JUSTIFICATION      \
             -e         "$BEHAVIOR"         \
             -h         $HEIGHT 
# note:
#   dzen did *NOT* like parsing my X11 geometry string when I tried to use -0
#   to put it at the bottom of the screen. I had to take the total screen
#   height (768) and subtract out the height of the dock (14) which is annoying
#   and hackish

# note2:
#   apparently BG_COLOR and FG_COLOR don't work well with dzen. When I used
#   those names to store the colors, dzen woulc choke completely spouting
#   errors that the vars were empty, but when I echo'd their values from within
#   the script, I would get the correct text back. But when I Changed the
#   variable names it magically started working. This could be a bash or sh
#   bug/feature
