#!/bin/bash
#

### globals
separator=" ^r(3x3) "
dzen_bitmaps="$HOME/local/dzen_bitmaps"
dzen_bitmaps2="$HOME/local/dzen_bitmaps2"

### email settings
email_newmail_fgcolor='red'
email_fgcolor=''

### clock settings
clock_fgcolor='#55CC55'
clock_icon_color='#FFFFFF'
clock_format="+%a %b %d %Y, %k:%M (%Z)"

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
#    all_mail=`get_all_mail "$mailbox"`
    ### output
    output="$label:$new_mail^fg()"
    if [ $new_mail -ne 0 ] && [ $3 -eq 1 ]; then
        output="^fg($email_newmail_fgcolor)$output"
    else
        output="^fg($email_fgcolor)$output"
    fi
    echo -n "$output"
}

function print_email()
{
    echo -n "^i($dzen_bitmaps/envelope.xbm) "
    process_mailbox "$HOME/mail/mlarc/Inbox" "inbox" 1
    echo -n " "
    process_mailbox "$HOME/mail/rtk/INBOX" "rtk" 1
}


function print_clock()
{
    echo -n "^fg($clock_icon_color)^i($dzen_bitmaps2/clock.xbm)^fg($clock_fgcolor) "
    echo -n `date "$clock_format"`
    echo -n "^fg()"
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
    echo "$clock_string$separator$email_string"

    # determine the shortest interval before the next update needs to occur
    # and then set that to the sleep interval
    interval=${intervals[0]}
    for i in ${intervals[@]}; do
        if [ $i -lt $interval ]; then
            interval=$i
        fi
    done
    sleep $interval
done
