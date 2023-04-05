#!/usr/bin/env bash
# Prepend a column with the weekday number: 1 (Mon) to 7 (Sun)
# -h -> human readable (Mon to Sun)

main() {
    declare weekDay start rest
    while read -r start rest; do
        if [[ $1 == -h ]]; then
            weekDay=$(LC_DATE=C date -d "$start" +%a)
        else
            weekDay=$(date -d "$start" +%u)
            # %w would be 0-6 (Sun to Sat)
        fi
        echo "$weekDay $start $rest"
    done
}

main "$@"
