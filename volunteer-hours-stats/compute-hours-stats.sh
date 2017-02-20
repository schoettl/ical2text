#!/bin/bash
# Compute volunteer hours statistics

printUsage() {
    cat <<EOF
usage: $PROGNAME [options] < file
       $PROGNAME [options] ICAL_URL

options:
    -t TYPE
        input type; one of ical, text, stat; default: ical
    -n  no categories, always use "all" instead of any category
    -s STAGE_NUMBER
        output stage; 1 is text, 2 is stat, 3 is final table output
    -h  print help message
EOF
}

readonly PROGNAME=$(basename "$0")
readonly PROGDIR=$(dirname "$(readlink -m "$0")")

# $*: command line arguments = "$@"
parseCommandLine() {
    declare stage=3
    declare inputType=ical
    while getopts "t:hns:" OPTION; do
         case $OPTION in
         h)
             printUsage
             exit 0
             ;;
         s)
             stage=$OPTARG
             ;;
         n)
             declare -gr NO_CATEGORIES=1
             ;;
         t)  inputType=$OPTARG
             ;;
        esac
    done
    shift $((OPTIND-1))

    declare -gr STAGE=$stage
    declare -gr INPUT_TYPE=$inputType

    if (( $# == 1 )); then
        if [[ $INPUT_TYPE =~ ^(ical|ics)$ ]]; then
            declare -rg ICAL_URL=$1
        else
            exitWithError "error: -t $INPUT_TYPE not allowed when iCal URL is given"
        fi
    fi

    if (( $# > 1 )); then
        printUsage
        exit 1
    fi

    return 0
}

# $1: error message
exitWithError() {
    echo "$1" >&2
    exit 1
}

outputIcs() {
    if [[ -n $ICAL_URL ]]; then
        curl -o- "$ICAL_URL"
    else
        cat
    fi
}

oneEventPerName() {
    awk -f one-event-per-description-line.awk
}

parseNames() {
    awk -vnoCategories="$NO_CATEGORIES" -f parse-names.awk
}

groupByAndSumUpHours() {
    if (( STAGE >= 2 )); then
        sort -t"	" -k5 -k4 --ignore-case \
            | awk -f group-by-name-and-category.awk \
            | sort -t"	" -k5 -k4 --ignore-case
    else
        cat
    fi
}

tableFormat() {
    if (( STAGE >= 3 )); then
        awk -f stat2table.awk
    else
        cat
    fi
}

main() {
    parseCommandLine "$@"

    case $INPUT_TYPE in
        ical|ics)
            outputIcs \
                | ical2text \
                | oneEventPerName \
                | parseNames \
                | groupByAndSumUpHours \
                | tableFormat
            ;;
        text|txt)
            oneEventPerName \
                | parseNames \
                | groupByAndSumUpHours \
                | tableFormat
            ;;
        stat)
            groupByAndSumUpHours \
                | tableFormat
            ;;
        *)
            exitWithError "error: invalid input type -t $INPUT_TYPE"
            ;;
    esac

}

main "$@"
