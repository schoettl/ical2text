# to be used after parse-names.awk and sort:
# sort -t"	" -k5 -k4 --ignore-case

# category and name is treated case-insensitive

# input format (tab separated):
# start_timestamp end_timestamp hours category name
# output format: as input format but sum(hours) group by category, name

function printResultLineForGroup(category, name, hoursSum) {
    print "_", "_", hoursSum, category, name
}

BEGIN {
    FS = "\t"
    OFS = "\t"
    firstLine = 1
}
{
    hours = $3
    category = $4
    name = $5
    if (firstLine) {
        firstLine = 0
        prevCategory = category
        prevName = name
    }
    if (tolower(category) == tolower(prevCategory) && tolower(name) == tolower(prevName)) {
        # Continue current group
        hoursSum += hours
    } else {
        # Switch to next group
        printResultLineForGroup(prevCategory, prevName, hoursSum)

        prevCategory = category
        prevName = name
        hoursSum = hours
    }
}
END {
    printResultLineForGroup(category, name, hoursSum)
}
