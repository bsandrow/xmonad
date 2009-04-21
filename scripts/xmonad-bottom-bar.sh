#!/bin/bash
#

### email settings
email_newmail_fgcolor='red'
email_fgcolor=''

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
clock_fgcolor='#55CC55'
clock_format="+^fg($clock_fgcolor)%a %b %d %Y^fg() | ^fg($clock_fgcolor)%k:%M (%Z)^fg() "

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
        echo -n "| ^fg($email_newmail_fgcolor)$label $new_mail/$all_mail^fg() "
    else
        echo -n "| ^fg($email_fgcolor)$label $new_mail/$all_mail^fg() "
    fi
}

function print_email()
{
    process_mailbox "$HOME/mail/gmail/INBOX" "gmail" 1
    process_mailbox "$HOME/mail/rtk/INBOX" "rtk" 1
}


function print_clock()
{
    echo -n `date "$clock_format"`
}

count=0
interval=0
intervals=(0 0)
clock_idx=0
email_idx=1
while true
do
    # update all intervals by the amount of time that has elapsed.
    i=0
    while [ $i -lt ${#intervals[*]} ];do
        intervals[$i]=$(( ${intervals[$i]} - $interval ))
        i=$(( $i + 1 ))
    done

    # update each peice of information when the interval comes around, reset
    # the interval counter when we reach the max interval value
    if [ ${intervals[$clock_idx]} -le 0 ]; then
        clock_string=`print_clock`
        unix_time=`date +%s`
        intervals[$clock_idx]=$(( 60 - $unix_time % 60 ))
    fi
    if [ ${intervals[$email_idx]} -le 0 ]; then
        email_string=`print_email`
        intervals[$email_idx]=60
    fi
    echo " $clock_string $email_string"

    # determine the shortest interval before the next update needs to occur
    # and then set that to the sleep interval
    interval=${intervals[0]}
    for i in ${intervals[@]}; do
        if [ $i -lt $interval ]; then
            interval=$i
        fi
    done
    sleep $interval
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
