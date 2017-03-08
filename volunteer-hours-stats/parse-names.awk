# to be used in a pipe after one-event-per-description-line.awk
# ical2text < ww-zeiterfassung.ics | sort | awk -f one-event-per-description-line.awk | awk -f parse-names.awk

# parameters (to be set with awk -vparam=1 ...):
# noCategories - if set to 1, there is only one category "all".
#   can be used to calculate the total hours for persons. default: 0

function trim(s) {
    return gensub(/^ +| +$/, "", "g", s)
}

BEGIN {
    OFS = "\t"
}
{
    split($0, text, "@@")
    person = gensub(/ +/, " ", "g", trim(text[2]))
    n = split(person, parts, " ")

    sHours = $3
    # Individuelle Stundenzahl von Person übernehmen, wenn gegeben
    if        (parts[3] ~ /[0-9,.]+/) {
        sHours = parts[3]
        parts[3] = ""
    } else if (parts[4] ~ /[0-9,.]+/) {
        sHours = parts[4]
        parts[4] = ""
    }
    hours = strtonum(gensub(/,/, ".", 1, sHours))

    category = $4
    if (category ~ /@@/) {
        print "warning: missing or malformed category for " text[1] > "/dev/stderr"
        next
    }

    # die Stundenzahl (wenn vorhanden) wurde gelöscht.
    # eines von beiden ist also der modifier (wenn vorhanden)
    # z.B. "tn" oder "a"
    categoryModifier = parts[3] parts[4]

    if (tolower(categoryModifier) == "tn") { # Teilnehmer
        if (category == "ausb" ||  category == "jrk" || category == "eh") {
            category = "alsTN"
        }
    } else { # Nicht Teilnehmer, also Ausbilder
        if (category == "übg") {
            category = "ausb"
        }
    }

    if (noCategories) {
        category = "all"
    }

    # timestamp hours category name rest
    print $1, $2, hours, category, parts[1] " " parts[2]
}
