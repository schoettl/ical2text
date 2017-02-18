# to be used in a pipe after one-event-per-description-line.awk
# ical2text < ww-zeiterfassung.ics | sort | awk -f one-event-per-description-line.awk | awk -f parse-names.awk

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
    $3 = strtonum(gensub(/,/, ".", 1, sHours))

    if ($4 ~ /@@/) {
        print "warning: missing tag for " text[1] > "/dev/stderr"
        next
    }
    print $1, $3, $4, parts[1] " " parts[2], parts[3] parts[4], text[1]
}
