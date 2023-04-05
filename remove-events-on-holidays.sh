#!/usr/bin/env bash
# Remove all events starting on holidays.

printUsage() {
    cat <<EOF
usage: $PROGNAME <icalendar-ics-file>
EOF
}

readonly PROGNAME=$(basename "$0")

# $1: error message
exitWithError() {
    echo "$1" >&2
    exit 1
}

main() {
    (( $# == 1 )) || exitWithError "$(printUsage)"
    declare holidaysFile=$1
    declare holidaysText
    holidaysText=$(mktemp)

    [[ -r $holidaysFile ]] || exitWithError "error: cannot open file for read: $holidaysFile"
    # this error cannot be catched below because of pipe and/or <

    # delimiter: "T" in "2017-01-04T19:30"
    ical2text < "$holidaysFile" | cut -dT -f1 | sed 's/^/^/' > "$holidaysText"
    # cat "$holidaysText"

    # print those that are not in the holidays pattern file
    grep -vf "$holidaysText"
}

main "$@"
