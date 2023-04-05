#!/usr/bin/env bash
# Remove events on weekends. Only the start time is considered!
# In other words: grep for events that take place Mo - Fr.

# $1: start date in ISO format
isWeekend() {
    declare dayOfWeek
    dayOfWeek=$(date -d "$1" +%u)
    (( dayOfWeek >= 6 )) # is weekend
}

main() {
    declare start rest
    while read -r start rest; do
        if ! isWeekend "$start"; then
            echo "$start $rest"
        fi
    done
}

main
