#!/usr/bin/env bash

# Sample shell script for using notify-send with matterhorn This script
# works on Linux only. It depends on the 'notify-send' command.

# Positional parameters passed to this script by Matterhorn:
mentioned="${1?}"
sender="${2?}"
message="${3?}"

# Script options

# notify_URGENCIES
#
# The first word is the urgency for items where you are not mentioned.
# The second word is the urgency for items where you are mentioned.
# Use "none" to not be notified; otherwise use "low", "normal", or
# "critical".
notify_URGENCIES="normal normal"

# The desktop notification category
notify_CATEGORY="im.received"

# Notification header
notify_HEAD="Matterhorn message from $sender"

# Notification body
notify_BODY="$message"

getUrgencyHelper() {
    if [ "$mentioned" == "1" ]
    then
        echo "$1"
    else
        if [ "$mentioned" == "2" ]
        then
            echo "$2"
        else
            echo "Error: mentioned value '$mentioned' unexpected" > /dev/stderr
            exit 1
        fi
    fi
}

getUrgency() {
    # We are using arguments as a poor man's bash array for portability
    # shellcheck disable=SC2086
    getUrgencyHelper $notify_URGENCIES
}

urgency=$(getUrgency)

if [ ! -z "$urgency" ]
then
    test "$urgency" = "none" ||
        notify-send -u "$urgency" -c "$notify_CATEGORY" "$notify_HEAD" "$notify_BODY"
fi
