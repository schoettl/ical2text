#!/bin/bash
# Prepend a column with the weekday number: 1 (Mon) to 7 (Sun)

main() {
    declare weekDay start rest
    while read -r start rest; do
        weekDay=$(date -d "$start" +%u)
        echo "$weekDay $start $rest"
    done
}

main
