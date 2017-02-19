# input must be stat, sorted by name

function printResultLineForGroup(name, hoursArr) {
    split(name, nameArr, " ")
    print nameArr[2], nameArr[1], hoursArr["alsTN"], hoursArr["freitag"], hoursArr["ausb"], hoursArr["orga"], hoursArr["seg"], hoursArr["abst"], hoursArr["wachd"]
}

BEGIN {
    FS = "\t"
    OFS = "\t"
    print "Name", "Vorname", "Teilnehmer", "Freitag", "Ausbilder", "Orga", "SEG", "Abstellung", "Wachdienst"
    firstLine = 1
}
{
    hours = $3
    category = $4
    categoryLower = tolower(category)
    name = $5

    if (firstLine) {
        firstLine = 0
        prevName = name
    }

    if (tolower(name) == tolower(prevName)) {
        # Continue current group
        if (hoursArr[categoryLower]) {
            print "warning: person " name " has multiple entries for category " category ". earlier hour values will be overwritten." > "/dev/stderr"
        }
        hoursArr[categoryLower] = hours
    } else {
        # Switch to next group
        printResultLineForGroup(prevName, hoursArr)

        prevName = name
        delete hoursArr
        hoursArr[categoryLower] = hours
    }
}
END {
    printResultLineForGroup(name, hoursArr)
}
