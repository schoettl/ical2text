function trim(s) {
    return gensub(/^ +| +$/, "", "g", s)
}
BEGIN {
    FS = "@@"
}
{
    if (trim($2) == "") {
        print "warning: no lines in event description - ignoring " $1 > "/dev/stderr"
        next
    }
    split($2, lines, ",,")
    for (i in lines) {
        line = trim(lines[i])
        if (line == "") next
        print $1 "@@" line "@@" $3
    }
}
