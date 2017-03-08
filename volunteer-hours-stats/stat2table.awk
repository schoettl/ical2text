# input must be stat, sorted by name

function printResultLineForGroup(name, hoursArr) {
    split(name, nameArr, " ")
    print nameArr[2], nameArr[1], hoursArr["alstn"], hoursArr["freitag"], hoursArr["ausb"], hoursArr["orga"], hoursArr["seg"], hoursArr["abst"], hoursArr["wachd"]
}

BEGIN {
    predefinedCategoriesStr = "freitag ausb orga seg abst wachd übg jrk eh"
    split(predefinedCategoriesStr, predefinedCategories, " ")

    FS = "\t"
    OFS = "\t"
    print "Name", "Vorname", "Teilnehmer", "Freitag", "Ausbilder", "Orga", "SEG", "Abstellung", "Wachdienst"
    firstLine = 1
}
{
    hours = $3
    category = tolower($4)
    name = $5

    if (firstLine) {
        firstLine = 0
        prevName = name
    }

    if (tolower(name) == tolower(prevName)) {
        # Continue current group
        if (hoursArr[category]) {
            print "warning: person " name " has multiple entries for category " category ". earlier hour values will be overwritten." > "/dev/stderr"
        }
        if (!(category in predefinedCategories)) {
            print "warning: category is not a predefined category: " category " (person " name "). ignoring this value." > "/dev/stderr"
        }
        hoursArr[category] = hours
    } else {
        # Switch to next group
        printResultLineForGroup(prevName, hoursArr)

        prevName = name
        delete hoursArr
        hoursArr[category] = hours
    }
}
END {
    if (!firstLine)
        printResultLineForGroup(prevName, hoursArr)
}
